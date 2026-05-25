#include <liblogit/logit.hpp>

int main() {
  auto LogIT = liblogit::LOGIT{};
  LogIT.localPath = "logs/cpp-basic.log";
  LogIT.level = liblogit::Level::DEBUG;
  LogIT(liblogit::Level::INFO) << "C++ app started";

  auto configured =
      liblogit::LOGIT::load_from_file("examples/config/v2-basic.json");
  configured.at("AppLog")(liblogit::Level::INFO) << "configured C++ log";
  return 0;
}
