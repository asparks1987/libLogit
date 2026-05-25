#include "liblogit.h"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if defined(_WIN32)
#include <direct.h>
#define LIBLOGIT_MKDIR(path) _mkdir(path)
#else
#include <sys/stat.h>
#define LIBLOGIT_MKDIR(path) mkdir(path, 0755)
#endif

enum { LIBLOGIT_OK = 0, LIBLOGIT_ERR = 1 };

static void copy_text(char *target, size_t target_size, const char *value) {
  if (target_size == 0) {
    return;
  }
  if (value == NULL) {
    target[0] = '\0';
    return;
  }
  snprintf(target, target_size, "%s", value);
}

static int looks_like_socket(const char *value) {
  return value != NULL &&
         (strncmp(value, "tcp://", 6) == 0 || strncmp(value, "udp://", 6) == 0);
}

static int strings_equal_ignore_case(const char *left, const char *right) {
  while (*left != '\0' && *right != '\0') {
    if (tolower((unsigned char)*left) != tolower((unsigned char)*right)) {
      return 0;
    }
    ++left;
    ++right;
  }
  return *left == '\0' && *right == '\0';
}

static const char *canonical_level_label(liblogit_level level) {
  switch (level) {
  case LIBLOGIT_TRACE:
    return "trace";
  case LIBLOGIT_DEBUG:
    return "debug";
  case LIBLOGIT_INFO:
    return "info";
  case LIBLOGIT_WARN:
    return "warn";
  case LIBLOGIT_ERROR:
    return "error";
  case LIBLOGIT_FATAL:
    return "fatal";
  }
  return "info";
}

static liblogit_level parse_level_or_info(const char *value) {
  if (value == NULL) {
    return LIBLOGIT_INFO;
  }
  if (strings_equal_ignore_case(value, "trace"))
    return LIBLOGIT_TRACE;
  if (strings_equal_ignore_case(value, "debug"))
    return LIBLOGIT_DEBUG;
  if (strings_equal_ignore_case(value, "info"))
    return LIBLOGIT_INFO;
  if (strings_equal_ignore_case(value, "warn") ||
      strings_equal_ignore_case(value, "warning"))
    return LIBLOGIT_WARN;
  if (strings_equal_ignore_case(value, "error"))
    return LIBLOGIT_ERROR;
  if (strings_equal_ignore_case(value, "fatal") ||
      strings_equal_ignore_case(value, "critical"))
    return LIBLOGIT_FATAL;
  return LIBLOGIT_INFO;
}

static void ensure_parent_dir(const char *target) {
  char buffer[512];
  size_t len;
  size_t i;

  if (target == NULL) {
    return;
  }

  copy_text(buffer, sizeof(buffer), target);
  len = strlen(buffer);
  for (i = 0; i < len; ++i) {
    if (buffer[i] == '/' || buffer[i] == '\\') {
      char saved = buffer[i];
      if (i == 0 || (i == 2 && buffer[1] == ':')) {
        continue;
      }
      buffer[i] = '\0';
      if (buffer[0] != '\0') {
        (void)LIBLOGIT_MKDIR(buffer);
      }
      buffer[i] = saved;
    }
  }
}

static int append_line(const char *target, const char *line) {
  FILE *file = NULL;
  if (target == NULL || target[0] == '\0') {
    return LIBLOGIT_OK;
  }
  ensure_parent_dir(target);
#if defined(_WIN32)
  if (fopen_s(&file, target, "a") != 0) {
    return LIBLOGIT_ERR;
  }
#else
  file = fopen(target, "a");
#endif
  if (file == NULL) {
    return LIBLOGIT_ERR;
  }
  fprintf(file, "%s\n", line);
  fclose(file);
  return LIBLOGIT_OK;
}

static void append_json_string(char *out, size_t out_size, const char *value) {
  size_t used = strlen(out);
  size_t i;
  if (used + 2 >= out_size) {
    return;
  }
  out[used++] = '"';
  out[used] = '\0';
  for (i = 0; value != NULL && value[i] != '\0' && used + 2 < out_size; ++i) {
    unsigned char ch = (unsigned char)value[i];
    if ((ch == '"' || ch == '\\') && used + 2 < out_size) {
      out[used++] = '\\';
      out[used++] = (char)ch;
    } else if (ch == '\n' && used + 2 < out_size) {
      out[used++] = '\\';
      out[used++] = 'n';
    } else if (ch == '\r' && used + 2 < out_size) {
      out[used++] = '\\';
      out[used++] = 'r';
    } else if (ch == '\t' && used + 2 < out_size) {
      out[used++] = '\\';
      out[used++] = 't';
    } else {
      out[used++] = (char)ch;
    }
    out[used] = '\0';
  }
  if (used + 1 < out_size) {
    out[used++] = '"';
    out[used] = '\0';
  }
}

static void append_text(char *out, size_t out_size, const char *value) {
  size_t used = strlen(out);
  size_t i = 0;
  if (used >= out_size) {
    return;
  }
  while (value != NULL && value[i] != '\0' && used + 1 < out_size) {
    out[used++] = value[i++];
  }
  out[used] = '\0';
}

static void render_message(const liblogit_logit *logit, liblogit_level level,
                           const char *message, char *out, size_t out_size) {
  char timestamp[64] = "";
  if (strings_equal_ignore_case(logit->format, "json")) {
    snprintf(out, out_size, "{\"level\":");
    append_json_string(out, out_size, canonical_level_label(level));
    append_text(out, out_size, ",\"logger\":");
    append_json_string(out, out_size, logit->name);
    append_text(out, out_size, ",\"message\":");
    append_json_string(out, out_size, message);
    if (logit->metadata_json[0] != '\0') {
      append_text(out, out_size, ",\"metadata\":");
      append_text(out, out_size, logit->metadata_json);
    }
    append_text(out, out_size, "}");
    return;
  }

  if (logit->timestamp) {
    time_t now = time(NULL);
    struct tm local_time;
#if defined(_WIN32)
    localtime_s(&local_time, &now);
#else
    localtime_r(&now, &local_time);
#endif
    strftime(timestamp, sizeof(timestamp), "%Y-%m-%dT%H:%M:%S", &local_time);
  }

  if (logit->timestamp && logit->tag_level) {
    snprintf(out, out_size, "%s %s %s", timestamp, liblogit_level_label(level),
             message);
  } else if (logit->timestamp) {
    snprintf(out, out_size, "%s %s", timestamp, message);
  } else if (logit->tag_level) {
    snprintf(out, out_size, "%s %s", liblogit_level_label(level), message);
  } else {
    snprintf(out, out_size, "%s", message);
  }
}

liblogit_logit liblogit_logit_default(void) {
  liblogit_logit logit;
  memset(&logit, 0, sizeof(logit));
  copy_text(logit.name, sizeof(logit.name), "default");
  logit.level = LIBLOGIT_INFO;
  logit.enabled = 1;
  logit.sink_console = 1;
  logit.timestamp = 1;
  logit.tag_level = 1;
  copy_text(logit.format, sizeof(logit.format), "text");
  return logit;
}

void liblogit_logit_set_name(liblogit_logit *logit, const char *name) {
  if (logit != NULL) {
    copy_text(logit->name, sizeof(logit->name), name);
  }
}

void liblogit_logit_set_local_path(liblogit_logit *logit, const char *path) {
  if (logit != NULL) {
    copy_text(logit->local_path, sizeof(logit->local_path), path);
    logit->sink_file = path != NULL && path[0] != '\0';
  }
}

void liblogit_logit_set_remote_path(liblogit_logit *logit, const char *path) {
  if (logit != NULL) {
    copy_text(logit->remote_path, sizeof(logit->remote_path), path);
    logit->sink_network = path != NULL && path[0] != '\0';
  }
}

void liblogit_logit_set_level(liblogit_logit *logit, liblogit_level level) {
  if (logit != NULL) {
    logit->level = level;
  }
}

void liblogit_logit_set_timestamp(liblogit_logit *logit, int enabled) {
  if (logit != NULL) {
    logit->timestamp = enabled != 0;
  }
}

void liblogit_logit_set_tag_level(liblogit_logit *logit, int enabled) {
  if (logit != NULL) {
    logit->tag_level = enabled != 0;
  }
}

void liblogit_logit_set_format(liblogit_logit *logit, const char *format) {
  if (logit != NULL) {
    copy_text(logit->format, sizeof(logit->format),
              format == NULL ? "text" : format);
  }
}

void liblogit_logit_set_metadata_json(liblogit_logit *logit,
                                      const char *metadata_json) {
  if (logit != NULL) {
    copy_text(logit->metadata_json, sizeof(logit->metadata_json),
              metadata_json);
  }
}

static char *read_all_text(const char *path) {
  FILE *file = NULL;
  long size;
  char *text;
#if defined(_WIN32)
  if (fopen_s(&file, path, "rb") != 0) {
    return NULL;
  }
#else
  file = fopen(path, "rb");
#endif
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
  while (cursor < end && isspace((unsigned char)*cursor)) {
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

static int extract_string(const char *start, const char *end, const char *key,
                          char *out, size_t out_size) {
  const char *cursor = value_start_for_key(start, end, key);
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
  if (out_size > 0) {
    out[used] = '\0';
  }
  return 1;
}

static int extract_bool(const char *start, const char *end, const char *key,
                        int *out) {
  const char *cursor = value_start_for_key(start, end, key);
  if (cursor == NULL) {
    return 0;
  }
  if (cursor + 4 <= end && strncmp(cursor, "true", 4) == 0) {
    *out = 1;
    return 1;
  }
  if (cursor + 5 <= end && strncmp(cursor, "false", 5) == 0) {
    *out = 0;
    return 1;
  }
  return 0;
}

static void compact_json(const char *start, const char *end, char *out,
                         size_t out_size) {
  size_t used = 0;
  int in_string = 0;
  int escape = 0;
  const char *cursor;
  for (cursor = start; cursor < end && used + 1 < out_size; ++cursor) {
    char ch = *cursor;
    if (in_string) {
      out[used++] = ch;
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
      out[used++] = ch;
    } else if (!isspace((unsigned char)ch)) {
      out[used++] = ch;
    }
  }
  out[used] = '\0';
}

static void apply_sinks(liblogit_logit *logit, const char *start,
                        const char *end) {
  const char *array_start;
  const char *array_end;
  char sink[32];
  const char *cursor;
  if (!extract_array(start, end, "sinks", &array_start, &array_end)) {
    return;
  }
  logit->sink_console = 0;
  logit->sink_file = 0;
  logit->sink_network = 0;
  cursor = array_start + 1;
  while (cursor < array_end) {
    const char *quote = strchr(cursor, '"');
    size_t len;
    if (quote == NULL || quote >= array_end) {
      break;
    }
    cursor = quote + 1;
    quote = strchr(cursor, '"');
    if (quote == NULL || quote >= array_end) {
      break;
    }
    len = (size_t)(quote - cursor);
    if (len >= sizeof(sink)) {
      len = sizeof(sink) - 1;
    }
    memcpy(sink, cursor, len);
    sink[len] = '\0';
    if (strings_equal_ignore_case(sink, "console"))
      logit->sink_console = 1;
    if (strings_equal_ignore_case(sink, "file"))
      logit->sink_file = 1;
    if (strings_equal_ignore_case(sink, "network"))
      logit->sink_network = 1;
    cursor = quote + 1;
  }
}

static void apply_object(liblogit_logit *logit, const char *start,
                         const char *end) {
  char value[512];
  int bool_value;
  const char *object_start;
  const char *object_end;

  if (extract_string(start, end, "name", value, sizeof(value))) {
    liblogit_logit_set_name(logit, value);
  }
  if (extract_string(start, end, "level", value, sizeof(value))) {
    liblogit_logit_set_level(logit, parse_level_or_info(value));
  }
  if (extract_bool(start, end, "timestamp", &bool_value)) {
    liblogit_logit_set_timestamp(logit, bool_value);
  }
  if (extract_bool(start, end, "tag_level", &bool_value)) {
    liblogit_logit_set_tag_level(logit, bool_value);
  }
  if (extract_bool(start, end, "enabled", &bool_value)) {
    logit->enabled = bool_value;
  }
  if (extract_string(start, end, "format", value, sizeof(value))) {
    liblogit_logit_set_format(logit, value);
  }
  if (extract_string(start, end, "path", value, sizeof(value)) ||
      extract_string(start, end, "localPath", value, sizeof(value)) ||
      extract_string(start, end, "local_path", value, sizeof(value)) ||
      extract_string(start, end, "file_location", value, sizeof(value))) {
    liblogit_logit_set_local_path(logit, value);
  }
  if (extract_string(start, end, "remotePath", value, sizeof(value)) ||
      extract_string(start, end, "remote_path", value, sizeof(value)) ||
      extract_string(start, end, "network_path", value, sizeof(value)) ||
      extract_string(start, end, "network_file_location", value,
                     sizeof(value))) {
    liblogit_logit_set_remote_path(logit, value);
  }
  if (extract_object(start, end, "metadata", &object_start, &object_end)) {
    compact_json(object_start, object_end, logit->metadata_json,
                 sizeof(logit->metadata_json));
  }
  apply_sinks(logit, start, end);
}

int liblogit_logit_load_from_file(const char *config_path, const char *name,
                                  liblogit_logit *out) {
  char *text;
  const char *root_start;
  const char *root_end;
  const char *defaults_start;
  const char *defaults_end;
  const char *logits_start;
  const char *logits_end;
  const char *logit_start;
  const char *logit_end;
  const char *target_name = name == NULL ? "default" : name;

  if (config_path == NULL || out == NULL) {
    return LIBLOGIT_ERR;
  }

  text = read_all_text(config_path);
  if (text == NULL) {
    return LIBLOGIT_ERR;
  }
  root_start = text;
  root_end = text + strlen(text);
  *out = liblogit_logit_default();

  if (extract_object(root_start, root_end, "logits", &logits_start,
                     &logits_end)) {
    if (extract_object(root_start, root_end, "defaults", &defaults_start,
                       &defaults_end)) {
      apply_object(out, defaults_start, defaults_end);
    }
    if (!extract_object(logits_start, logits_end, target_name, &logit_start,
                        &logit_end)) {
      free(text);
      return LIBLOGIT_ERR;
    }
    liblogit_logit_set_name(out, target_name);
    apply_object(out, logit_start, logit_end);
  } else {
    apply_object(out, root_start, root_end);
  }

  free(text);
  return LIBLOGIT_OK;
}

liblogit_builder liblogit_logit_at(liblogit_logit *logit,
                                   liblogit_level level) {
  liblogit_builder builder;
  memset(&builder, 0, sizeof(builder));
  builder.logit = logit;
  builder.level = level;
  return builder;
}

int liblogit_builder_append(liblogit_builder *builder, const char *fragment) {
  size_t available;
  int written;

  if (builder == NULL || builder->committed || fragment == NULL) {
    return LIBLOGIT_OK;
  }
  available = sizeof(builder->buffer) - builder->length;
  if (available == 0) {
    return LIBLOGIT_ERR;
  }
  written =
      snprintf(builder->buffer + builder->length, available, "%s", fragment);
  if (written < 0 || (size_t)written >= available) {
    builder->length = sizeof(builder->buffer) - 1;
    return LIBLOGIT_ERR;
  }
  builder->length += (size_t)written;
  return LIBLOGIT_OK;
}

int liblogit_builder_commit(liblogit_builder *builder) {
  if (builder == NULL || builder->committed) {
    return LIBLOGIT_OK;
  }
  builder->committed = 1;
  if (builder->length == 0) {
    return LIBLOGIT_OK;
  }
  return liblogit_logit_log(builder->logit, builder->level, builder->buffer);
}

int liblogit_logit_log(liblogit_logit *logit, liblogit_level level,
                       const char *message) {
  char rendered[2300];
  int status = LIBLOGIT_OK;

  if (logit == NULL || message == NULL || !logit->enabled ||
      level < logit->level) {
    return LIBLOGIT_OK;
  }

  render_message(logit, level, message, rendered, sizeof(rendered));
  if (logit->sink_console) {
    fprintf(stderr, "%s\n", rendered);
  }
  if (logit->sink_file) {
    status |= append_line(logit->local_path, rendered);
  }
  if (logit->sink_network && !looks_like_socket(logit->remote_path)) {
    status |= append_line(logit->remote_path, rendered);
  }
  return status;
}

const char *liblogit_level_label(liblogit_level level) {
  switch (level) {
  case LIBLOGIT_TRACE:
    return "TRACE";
  case LIBLOGIT_DEBUG:
    return "DEBUG";
  case LIBLOGIT_INFO:
    return "INFO";
  case LIBLOGIT_WARN:
    return "WARNING";
  case LIBLOGIT_ERROR:
    return "ERROR";
  case LIBLOGIT_FATAL:
    return "CRITICAL";
  }
  return "INFO";
}
