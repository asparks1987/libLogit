#include <liblogit/logit.h>

int main(void) {
  liblogit_logit logit = liblogit_logit_default();
  liblogit_logit configured;
  liblogit_builder builder;

  liblogit_logit_set_local_path(&logit, "logs/c-app.log");
  liblogit_logit_set_level(&logit, LIBLOGIT_DEBUG);

  builder = liblogit_logit_at(&logit, LIBLOGIT_INFO);
  liblogit_builder_append(&builder, "C app started");
  if (liblogit_builder_commit(&builder) != 0) {
    return 1;
  }

  if (liblogit_logit_load_from_file("examples/config/v2-basic.json", "AppLog",
                                    &configured) != 0) {
    return 1;
  }
  return liblogit_logit_log(&configured, LIBLOGIT_INFO, "configured C log");
}
