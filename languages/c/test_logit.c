#include "liblogit.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#include <direct.h>
#include <windows.h>
#define TEST_MKDIR(path) _mkdir(path)
#define TEST_FIXTURE_DIR "tests\\conformance\\fixtures"
#define TEST_PATH_SEP "\\"
#else
#include <dirent.h>
#include <sys/stat.h>
#define TEST_MKDIR(path) mkdir(path, 0755)
#define TEST_FIXTURE_DIR "tests/conformance/fixtures"
#define TEST_PATH_SEP "/"
#endif

#define MAX_EXPECTED_FILES 8
#define MAX_EXPECTED_LINES 16
#define MAX_MESSAGES 16
#define MAX_FRAGMENTS 8

typedef struct expected_file {
  char path[256];
  char lines[MAX_EXPECTED_LINES][512];
  int line_count;
} expected_file;

typedef struct fixture_message {
  char logger[64];
  liblogit_level level;
  char fragments[MAX_FRAGMENTS][512];
  int fragment_count;
} fixture_message;

static FILE *open_file(const char *path, const char *mode) {
  FILE *file = NULL;
#if defined(_WIN32)
  (void)fopen_s(&file, path, mode);
#else
  file = fopen(path, mode);
#endif
  return file;
}

static int file_contains(const char *path, const char *needle) {
  char buffer[512];
  FILE *file = open_file(path, "r");
  if (file == NULL) {
    return 0;
  }
  while (fgets(buffer, sizeof(buffer), file) != NULL) {
    if (strstr(buffer, needle) != NULL) {
      fclose(file);
      return 1;
    }
  }
  fclose(file);
  return 0;
}

static char *read_all_text(const char *path) {
  FILE *file = open_file(path, "rb");
  long size;
  char *text;

  if (file == NULL) {
    return NULL;
  }
  if (fseek(file, 0, SEEK_END) != 0) {
    fclose(file);
    return NULL;
  }
  size = ftell(file);
  if (size < 0 || fseek(file, 0, SEEK_SET) != 0) {
    fclose(file);
    return NULL;
  }
  text = (char *)malloc((size_t)size + 1);
  if (text == NULL) {
    fclose(file);
    return NULL;
  }
  if (fread(text, 1, (size_t)size, file) != (size_t)size) {
    free(text);
    fclose(file);
    return NULL;
  }
  text[size] = '\0';
  fclose(file);
  return text;
}

static const char *skip_ws(const char *cursor, const char *end) {
  while (cursor < end && (*cursor == ' ' || *cursor == '\n' ||
                          *cursor == '\r' || *cursor == '\t')) {
    ++cursor;
  }
  return cursor;
}

static const char *find_key(const char *start, const char *end,
                            const char *key) {
  char pattern[96];
  const char *cursor = start;
  snprintf(pattern, sizeof(pattern), "\"%s\"", key);
  while (cursor != NULL && cursor < end) {
    cursor = strstr(cursor, pattern);
    if (cursor == NULL || cursor >= end) {
      return NULL;
    }
    return cursor + strlen(pattern);
  }
  return NULL;
}

static const char *value_start_for_key(const char *start, const char *end,
                                       const char *key) {
  const char *cursor = find_key(start, end, key);
  if (cursor == NULL) {
    return NULL;
  }
  cursor = skip_ws(cursor, end);
  if (cursor >= end || *cursor != ':') {
    return NULL;
  }
  return skip_ws(cursor + 1, end);
}

static const char *matching_json_end(const char *open, const char *end) {
  char opener = *open;
  char closer = opener == '{' ? '}' : ']';
  int depth = 0;
  int in_string = 0;
  int escape = 0;
  const char *cursor;

  for (cursor = open; cursor < end; ++cursor) {
    char ch = *cursor;
    if (in_string) {
      if (escape) {
        escape = 0;
      } else if (ch == '\\') {
        escape = 1;
      } else if (ch == '"') {
        in_string = 0;
      }
      continue;
    }
    if (ch == '"') {
      in_string = 1;
    } else if (ch == opener) {
      ++depth;
    } else if (ch == closer) {
      --depth;
      if (depth == 0) {
        return cursor + 1;
      }
    }
  }
  return NULL;
}

static int extract_object(const char *start, const char *end, const char *key,
                          const char **object_start, const char **object_end) {
  const char *value = value_start_for_key(start, end, key);
  if (value == NULL || value >= end || *value != '{') {
    return 0;
  }
  *object_start = value;
  *object_end = matching_json_end(value, end);
  return *object_end != NULL;
}

static int extract_array(const char *start, const char *end, const char *key,
                         const char **array_start, const char **array_end) {
  const char *value = value_start_for_key(start, end, key);
  if (value == NULL || value >= end || *value != '[') {
    return 0;
  }
  *array_start = value;
  *array_end = matching_json_end(value, end);
  return *array_end != NULL;
}

static int parse_json_string(const char *start, const char *end, char *out,
                             size_t out_size, const char **next) {
  const char *cursor = start;
  size_t used = 0;

  if (cursor == NULL || cursor >= end || *cursor != '"') {
    return 0;
  }
  ++cursor;
  while (cursor < end && *cursor != '"') {
    char ch = *cursor++;
    if (ch == '\\' && cursor < end) {
      ch = *cursor++;
      if (ch == 'n')
        ch = '\n';
      else if (ch == 'r')
        ch = '\r';
      else if (ch == 't')
        ch = '\t';
    }
    if (used + 1 < out_size) {
      out[used++] = ch;
    }
  }
  if (cursor >= end || *cursor != '"') {
    return 0;
  }
  if (out_size > 0) {
    out[used] = '\0';
  }
  if (next != NULL) {
    *next = cursor + 1;
  }
  return 1;
}

static int extract_string(const char *start, const char *end, const char *key,
                          char *out, size_t out_size) {
  const char *cursor = value_start_for_key(start, end, key);
  return parse_json_string(cursor, end, out, out_size, NULL);
}

static liblogit_level parse_test_level(const char *value) {
  if (strcmp(value, "trace") == 0)
    return LIBLOGIT_TRACE;
  if (strcmp(value, "debug") == 0)
    return LIBLOGIT_DEBUG;
  if (strcmp(value, "warn") == 0 || strcmp(value, "warning") == 0)
    return LIBLOGIT_WARN;
  if (strcmp(value, "error") == 0)
    return LIBLOGIT_ERROR;
  if (strcmp(value, "fatal") == 0 || strcmp(value, "critical") == 0)
    return LIBLOGIT_FATAL;
  return LIBLOGIT_INFO;
}

static int expected_path_index(const expected_file *expected,
                               int expected_count, const char *value) {
  int i;
  for (i = 0; i < expected_count; ++i) {
    if (strcmp(expected[i].path, value) == 0) {
      return i;
    }
  }
  return -1;
}

static int parse_expected_files(const char *start, const char *end,
                                expected_file *expected, int *expected_count) {
  const char *object_start;
  const char *object_end;
  const char *cursor;
  int count = 0;

  if (!extract_object(start, end, "expected_files", &object_start,
                      &object_end)) {
    return 0;
  }
  cursor = object_start + 1;
  while (cursor < object_end && count < MAX_EXPECTED_FILES) {
    const char *array_start;
    const char *array_end;
    cursor = skip_ws(cursor, object_end);
    if (cursor >= object_end || *cursor == '}') {
      break;
    }
    if (*cursor == ',') {
      ++cursor;
      continue;
    }
    if (!parse_json_string(cursor, object_end, expected[count].path,
                           sizeof(expected[count].path), &cursor)) {
      return 0;
    }
    cursor = skip_ws(cursor, object_end);
    if (cursor >= object_end || *cursor != ':') {
      return 0;
    }
    cursor = skip_ws(cursor + 1, object_end);
    if (cursor >= object_end || *cursor != '[') {
      return 0;
    }
    array_start = cursor;
    array_end = matching_json_end(array_start, object_end);
    if (array_end == NULL) {
      return 0;
    }
    expected[count].line_count = 0;
    cursor = array_start + 1;
    while (cursor < array_end &&
           expected[count].line_count < MAX_EXPECTED_LINES) {
      cursor = skip_ws(cursor, array_end);
      if (cursor >= array_end || *cursor == ']') {
        break;
      }
      if (*cursor == ',') {
        ++cursor;
        continue;
      }
      if (!parse_json_string(
              cursor, array_end,
              expected[count].lines[expected[count].line_count],
              sizeof(expected[count].lines[expected[count].line_count]),
              &cursor)) {
        return 0;
      }
      expected[count].line_count += 1;
    }
    count += 1;
    cursor = array_end;
  }
  *expected_count = count;
  return count > 0;
}

static int parse_messages(const char *start, const char *end,
                          fixture_message *messages, int *message_count) {
  const char *array_start;
  const char *array_end;
  const char *cursor;
  int count = 0;

  if (!extract_array(start, end, "messages", &array_start, &array_end)) {
    return 0;
  }
  cursor = array_start + 1;
  while (cursor < array_end && count < MAX_MESSAGES) {
    const char *message_start;
    const char *message_end;
    const char *fragments_start;
    const char *fragments_end;
    char level[32];

    cursor = skip_ws(cursor, array_end);
    if (cursor >= array_end || *cursor == ']') {
      break;
    }
    if (*cursor == ',') {
      ++cursor;
      continue;
    }
    if (*cursor != '{') {
      return 0;
    }
    message_start = cursor;
    message_end = matching_json_end(message_start, array_end);
    if (message_end == NULL ||
        !extract_string(message_start, message_end, "logger",
                        messages[count].logger,
                        sizeof(messages[count].logger)) ||
        !extract_string(message_start, message_end, "level", level,
                        sizeof(level)) ||
        !extract_array(message_start, message_end, "fragments",
                       &fragments_start, &fragments_end)) {
      return 0;
    }
    messages[count].level = parse_test_level(level);
    messages[count].fragment_count = 0;
    cursor = fragments_start + 1;
    while (cursor < fragments_end &&
           messages[count].fragment_count < MAX_FRAGMENTS) {
      cursor = skip_ws(cursor, fragments_end);
      if (cursor >= fragments_end || *cursor == ']') {
        break;
      }
      if (*cursor == ',') {
        ++cursor;
        continue;
      }
      if (!parse_json_string(
              cursor, fragments_end,
              messages[count].fragments[messages[count].fragment_count],
              sizeof(messages[count].fragments[messages[count].fragment_count]),
              &cursor)) {
        return 0;
      }
      messages[count].fragment_count += 1;
    }
    count += 1;
    cursor = message_end;
  }
  *message_count = count;
  return count > 0;
}

static int write_rewritten_config(const char *config_path, const char *root_dir,
                                  const char *config_start,
                                  const char *config_end,
                                  const expected_file *expected,
                                  int expected_count) {
  FILE *file = open_file(config_path, "w");
  const char *cursor = config_start;

  if (file == NULL) {
    return 0;
  }
  while (cursor < config_end) {
    if (*cursor == '"') {
      const char *next = NULL;
      char value[256];
      if (!parse_json_string(cursor, config_end, value, sizeof(value), &next)) {
        fclose(file);
        return 0;
      }
      if (expected_path_index(expected, expected_count, value) >= 0) {
        fprintf(file, "\"%s/%s\"", root_dir, value);
      } else {
        fwrite(cursor, 1, (size_t)(next - cursor), file);
      }
      cursor = next;
    } else {
      fputc(*cursor, file);
      ++cursor;
    }
  }
  fclose(file);
  return 1;
}

static int compare_expected_file(const char *root_dir,
                                 const expected_file *expected) {
  char path[512];
  char line[512];
  FILE *file;
  int index = 0;

  snprintf(path, sizeof(path), "%s/%s", root_dir, expected->path);
  file = open_file(path, "r");
  if (file == NULL) {
    fprintf(stderr, "C shared fixture expected file was not written: %s\n",
            path);
    return 1;
  }
  while (fgets(line, sizeof(line), file) != NULL) {
    size_t len = strlen(line);
    while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r')) {
      line[--len] = '\0';
    }
    if (index >= expected->line_count ||
        strcmp(line, expected->lines[index]) != 0) {
      fprintf(stderr, "C shared fixture output mismatch in %s at line %d\n",
              path, index + 1);
      fclose(file);
      return 1;
    }
    index += 1;
  }
  fclose(file);
  if (index != expected->line_count) {
    fprintf(stderr, "C shared fixture output line count mismatch in %s\n",
            path);
    return 1;
  }
  return 0;
}

static int run_shared_fixture(const char *fixture_path) {
  char *text = read_all_text(fixture_path);
  const char *root_start;
  const char *root_end;
  const char *config_start;
  const char *config_end;
  char name[64];
  char root_dir[256];
  char config_path[320];
  expected_file expected[MAX_EXPECTED_FILES];
  fixture_message messages[MAX_MESSAGES];
  int expected_count = 0;
  int message_count = 0;
  int i;

  if (text == NULL) {
    fprintf(stderr, "Could not read C shared fixture: %s\n", fixture_path);
    return 1;
  }

  root_start = text;
  root_end = text + strlen(text);
  if (!extract_string(root_start, root_end, "name", name, sizeof(name)) ||
      !extract_object(root_start, root_end, "config", &config_start,
                      &config_end) ||
      !parse_expected_files(root_start, root_end, expected, &expected_count) ||
      !parse_messages(root_start, root_end, messages, &message_count)) {
    fprintf(stderr, "Could not parse C shared fixture: %s\n", fixture_path);
    free(text);
    return 1;
  }

  snprintf(root_dir, sizeof(root_dir), "logs/c-conformance/%s", name);
  snprintf(config_path, sizeof(config_path), "%s/logit.json", root_dir);
  (void)TEST_MKDIR("logs");
  (void)TEST_MKDIR("logs/c-conformance");
  (void)TEST_MKDIR(root_dir);

  for (i = 0; i < expected_count; ++i) {
    char output_path[512];
    snprintf(output_path, sizeof(output_path), "%s/%s", root_dir,
             expected[i].path);
    remove(output_path);
  }
  if (!write_rewritten_config(config_path, root_dir, config_start, config_end,
                              expected, expected_count)) {
    fprintf(stderr, "Could not write C rewritten fixture config: %s\n",
            config_path);
    free(text);
    return 1;
  }

  for (i = 0; i < message_count; ++i) {
    liblogit_logit logit;
    liblogit_builder builder;
    int fragment_index;

    if (liblogit_logit_load_from_file(config_path, messages[i].logger,
                                      &logit) != 0) {
      fprintf(stderr, "C fixture %s could not load logger %s\n", name,
              messages[i].logger);
      free(text);
      return 1;
    }
    builder = liblogit_logit_at(&logit, messages[i].level);
    for (fragment_index = 0; fragment_index < messages[i].fragment_count;
         ++fragment_index) {
      liblogit_builder_append(&builder, messages[i].fragments[fragment_index]);
    }
    if (liblogit_builder_commit(&builder) != 0) {
      fprintf(stderr, "C fixture %s failed while logging message %d\n", name,
              i + 1);
      free(text);
      return 1;
    }
  }

  for (i = 0; i < expected_count; ++i) {
    if (compare_expected_file(root_dir, &expected[i]) != 0) {
      free(text);
      return 1;
    }
  }

  free(text);
  return 0;
}

static int path_ends_with(const char *value, const char *suffix) {
  size_t value_len = strlen(value);
  size_t suffix_len = strlen(suffix);
  return value_len >= suffix_len &&
         strcmp(value + value_len - suffix_len, suffix) == 0;
}

static int run_shared_fixtures(void) {
  int count = 0;
#if defined(_WIN32)
  char pattern[512];
  WIN32_FIND_DATAA data;
  HANDLE handle;

  snprintf(pattern, sizeof(pattern), "%s\\*.json", TEST_FIXTURE_DIR);
  handle = FindFirstFileA(pattern, &data);
  if (handle == INVALID_HANDLE_VALUE) {
    fprintf(stderr, "Could not open C shared fixture directory: %s\n",
            TEST_FIXTURE_DIR);
    return 1;
  }
  do {
    if ((data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0 &&
        path_ends_with(data.cFileName, ".json")) {
      char path[512];
      snprintf(path, sizeof(path), "%s%s%s", TEST_FIXTURE_DIR, TEST_PATH_SEP,
               data.cFileName);
      if (run_shared_fixture(path) != 0) {
        FindClose(handle);
        return 1;
      }
      count += 1;
    }
  } while (FindNextFileA(handle, &data));
  FindClose(handle);
#else
  DIR *dir = opendir(TEST_FIXTURE_DIR);
  struct dirent *entry;

  if (dir == NULL) {
    fprintf(stderr, "Could not open C shared fixture directory: %s\n",
            TEST_FIXTURE_DIR);
    return 1;
  }
  while ((entry = readdir(dir)) != NULL) {
    if (path_ends_with(entry->d_name, ".json")) {
      char path[512];
      snprintf(path, sizeof(path), "%s%s%s", TEST_FIXTURE_DIR, TEST_PATH_SEP,
               entry->d_name);
      if (run_shared_fixture(path) != 0) {
        closedir(dir);
        return 1;
      }
      count += 1;
    }
  }
  closedir(dir);
#endif
  if (count == 0) {
    fprintf(stderr, "No C shared conformance fixtures were found\n");
    return 1;
  }
  return 0;
}

int main(void) {
  const char *path = "logs/c-binding-test.log";
  liblogit_logit logit = liblogit_logit_default();
  liblogit_builder builder;
  (void)TEST_MKDIR("logs");
  remove(path);

  liblogit_logit_set_name(&logit, "CLog");
  liblogit_logit_set_local_path(&logit, path);
  liblogit_logit_set_level(&logit, LIBLOGIT_DEBUG);
  liblogit_logit_set_timestamp(&logit, 0);
  logit.sink_console = 0;

  builder = liblogit_logit_at(&logit, LIBLOGIT_DEBUG);
  liblogit_builder_append(&builder, "hello");
  liblogit_builder_append(&builder, " c");
  if (liblogit_builder_commit(&builder) != 0) {
    return 1;
  }

  if (!file_contains(path, "DEBUG hello c")) {
    fprintf(stderr, "expected C log output was missing\n");
    return 1;
  }

  liblogit_logit_log(&logit, LIBLOGIT_TRACE, "hidden");
  if (file_contains(path, "hidden")) {
    fprintf(stderr, "below-threshold C log output was emitted\n");
    return 1;
  }

  if (run_shared_fixtures() != 0) {
    return 1;
  }

  printf("C libLogit tests passed\n");
  return 0;
}
