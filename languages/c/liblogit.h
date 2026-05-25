#ifndef LIBLOGIT_C_H
#define LIBLOGIT_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>

typedef enum liblogit_level {
  LIBLOGIT_TRACE = 0,
  LIBLOGIT_DEBUG = 1,
  LIBLOGIT_INFO = 2,
  LIBLOGIT_WARN = 3,
  LIBLOGIT_ERROR = 4,
  LIBLOGIT_FATAL = 5
} liblogit_level;

typedef struct liblogit_logit {
  char name[64];
  char local_path[512];
  char remote_path[512];
  liblogit_level level;
  int enabled;
  int sink_console;
  int sink_file;
  int sink_network;
  int timestamp;
  int tag_level;
  char format[16];
  char metadata_json[512];
} liblogit_logit;

typedef struct liblogit_builder {
  liblogit_logit *logit;
  liblogit_level level;
  char buffer[2048];
  size_t length;
  int committed;
} liblogit_builder;

liblogit_logit liblogit_logit_default(void);
void liblogit_logit_set_name(liblogit_logit *logit, const char *name);
void liblogit_logit_set_local_path(liblogit_logit *logit, const char *path);
void liblogit_logit_set_remote_path(liblogit_logit *logit, const char *path);
void liblogit_logit_set_level(liblogit_logit *logit, liblogit_level level);
void liblogit_logit_set_timestamp(liblogit_logit *logit, int enabled);
void liblogit_logit_set_tag_level(liblogit_logit *logit, int enabled);
void liblogit_logit_set_format(liblogit_logit *logit, const char *format);
void liblogit_logit_set_metadata_json(liblogit_logit *logit,
                                      const char *metadata_json);
int liblogit_logit_load_from_file(const char *config_path, const char *name,
                                  liblogit_logit *out);
liblogit_builder liblogit_logit_at(liblogit_logit *logit, liblogit_level level);
int liblogit_builder_append(liblogit_builder *builder, const char *fragment);
int liblogit_builder_commit(liblogit_builder *builder);
int liblogit_logit_log(liblogit_logit *logit, liblogit_level level,
                       const char *message);
const char *liblogit_level_label(liblogit_level level);

#ifdef __cplusplus
}
#endif

#endif
