#include "../../libLogit.h"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

bool file_contains(const std::filesystem::path &path,
                   const std::string &needle) {
  std::ifstream input(path);
  std::string line;
  while (std::getline(input, line)) {
    if (line.find(needle) != std::string::npos) {
      return true;
    }
  }
  return false;
}

std::string read_file(const std::filesystem::path &path) {
  std::ifstream input(path);
  std::ostringstream buffer;
  buffer << input.rdbuf();
  return buffer.str();
}

void write_file(const std::filesystem::path &path, const std::string &body) {
  if (!path.parent_path().empty()) {
    std::filesystem::create_directories(path.parent_path());
  }
  std::ofstream output(path);
  output << body;
}

class CurrentPathGuard {
public:
  explicit CurrentPathGuard(const std::filesystem::path &next)
      : previous_(std::filesystem::current_path()) {
    std::filesystem::current_path(next);
  }

  ~CurrentPathGuard() { std::filesystem::current_path(previous_); }

private:
  std::filesystem::path previous_;
};

std::string fragment_to_string(const nlohmann::json &fragment) {
  return fragment.is_string() ? fragment.get<std::string>() : fragment.dump();
}

void run_shared_fixture(const std::filesystem::path &fixture_path) {
  std::ifstream input(fixture_path);
  if (!input.is_open()) {
    throw std::runtime_error("failed to open fixture: " +
                             fixture_path.string());
  }

  const nlohmann::json fixture = nlohmann::json::parse(input);
  const std::string name = fixture.at("name").get<std::string>();
  const std::filesystem::path root =
      std::filesystem::path("logs/cpp-conformance") / name;
  std::filesystem::remove_all(root);
  std::filesystem::create_directories(root);
  write_file(root / "config.json", fixture.at("config").dump(2));

  {
    CurrentPathGuard guard(root);
    auto logits = liblogit::LOGIT::load_from_file("config.json");
    for (const auto &message : fixture.at("messages")) {
      auto &logit = logits.at(message.at("logger").get<std::string>());
      auto builder = logit(
          liblogit::LOGIT::parse_level(message.at("level").get<std::string>()));
      for (const auto &fragment : message.at("fragments")) {
        builder << fragment_to_string(fragment);
      }
      builder.commit();
    }
  }

  for (const auto &expected_file : fixture.at("expected_files").items()) {
    const std::string actual = read_file(root / expected_file.key());
    for (const auto &expected_line : expected_file.value()) {
      const std::string expected = expected_line.get<std::string>();
      if (actual.find(expected) == std::string::npos) {
        throw std::runtime_error("fixture " + name +
                                 " missing expected line: " + expected);
      }
    }
  }
}

} // namespace

int main() {
  const std::filesystem::path root = "logs/cpp-binding-test";
  std::filesystem::remove_all(root);
  std::filesystem::create_directories(root);

  liblogit::LOGIT direct;
  direct.name = "CppLog";
  direct.localPath = (root / "direct.log").string();
  direct.sinks = {"file"};
  direct.level = liblogit::Level::DEBUG;
  direct.timestamp = false;

  direct(liblogit::Level::DEBUG) << "hello" << " cpp";
  direct(liblogit::Level::TRACE) << "hidden";

  if (!file_contains(root / "direct.log", "DEBUG hello cpp")) {
    std::cerr << "expected C++ direct log output was missing\n";
    return 1;
  }
  if (file_contains(root / "direct.log", "hidden")) {
    std::cerr << "below-threshold C++ log output was emitted\n";
    return 1;
  }

  const std::filesystem::path config_path = root / "config.json";
  write_file(config_path, R"json({
  "version": "0.2",
  "defaults": {
    "level": "info",
    "timestamp": false,
    "tag_level": true,
    "format": "text",
    "sinks": ["file"]
  },
  "logits": {
    "AppLog": {
      "path": "logs/cpp-binding-test/configured.log",
      "level": "debug"
    },
    "AuditLog": {
      "path": "logs/cpp-binding-test/audit.jsonl",
      "format": "json",
      "metadata": {
        "component": "audit"
      }
    }
  }
})json");

  auto logits = liblogit::LOGIT::load_from_file(config_path);
  logits.at("AppLog")(liblogit::Level::INFO) << "configured";
  logits.at("AuditLog")(liblogit::Level::INFO) << "user signed in";

  if (!file_contains(root / "configured.log", "INFO configured")) {
    std::cerr << "expected C++ config-loaded log output was missing\n";
    return 1;
  }

  const std::string audit = read_file(root / "audit.jsonl");
  if (audit.find(
          R"json({"level":"info","logger":"AuditLog","message":"user signed in","metadata":{"component":"audit"}})json") ==
      std::string::npos) {
    std::cerr << "expected C++ JSON log output was missing\n";
    return 1;
  }

  try {
    for (const auto &fixture : {
             "tests/conformance/fixtures/basic_text_file.json",
             "tests/conformance/fixtures/level_filtering.json",
             "tests/conformance/fixtures/json_metadata.json",
         }) {
      run_shared_fixture(fixture);
    }
  } catch (const std::exception &exc) {
    std::cerr << exc.what() << '\n';
    return 1;
  }

  std::cout << "C++ libLogit tests passed\n";
  return 0;
}
