#ifndef LIBLOGIT_H
#define LIBLOGIT_H

#include <chrono>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <mutex>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <cctype>

// Requires nlohmann/json.hpp (https://github.com/nlohmann/json)
#include <nlohmann/json.hpp>

namespace liblogit {

/** Severity levels exposed to consumers of the logging API. */
enum class Level { TRACE, DEBUG, INFO, WARN, ERROR, FATAL };

/**
 * Parsed configuration shared by all sinks.
 * Only the fields defined in the phase-one JSON schema are represented here.
 */
struct Config {
    Level threshold = Level::INFO;
    bool tag_level = true;
    bool timestamp = true;
    std::optional<std::string> file_location;
    std::optional<std::string> network_file_location;

    /** Load configuration data from a JSON file residing at *path*. */
    static Config load_from_file(const std::filesystem::path& path);
};

/** Core logging façade that routes messages to configured sinks. */
class Logger {
public:
    /** Replace the active logging configuration at runtime. */
    static void configure(const Config& config);
    /** Convenience helper that loads configuration and then configures the logger. */
    static void configure_from_file(const std::filesystem::path& path);
    /** Emit *message* at the requested *level* using all configured sinks. */
    static void emit(Level level, std::string_view message);

    static std::string level_to_string(Level level);
    static Level parse_level(std::string_view name);

private:
    static bool should_emit(Level level);
    static std::string build_prefix(Level level);
    static void write_to_file(const std::optional<std::string>& path, std::string_view payload);
    static void write_to_socket(std::string_view target, std::string_view payload);

    static inline std::mutex mutex_{};
    static inline Config config_{};
    static inline bool configured_ = false;
};

/** Tiny RAII helper that mimics the LOG(level) << streaming syntax. */
class LogMessage {
public:
    explicit LogMessage(Level level) : level_(level) {}
    ~LogMessage() { flush(); }

    template <typename T>
    LogMessage& operator<<(const T& value) {
        if (!committed_) {
            stream_ << value;
        }
        return *this;
    }

    LogMessage& operator<<(std::ostream& (*manip)(std::ostream&)) {
        if (!committed_) {
            manip(stream_);
        }
        return *this;
    }

    void commit() {
        if (!committed_) {
            committed_ = true;
            Logger::emit(level_, stream_.str());
        }
    }

private:
    void flush() {
        commit();
    }

    Level level_;
    std::ostringstream stream_;
    bool committed_ = false;
};

inline int level_rank(Level level) {
    switch (level) {
        case Level::TRACE: return 0;
        case Level::DEBUG: return 1;
        case Level::INFO: return 2;
        case Level::WARN: return 3;
        case Level::ERROR: return 4;
        case Level::FATAL: return 5;
    }
    return 5;
}

inline Config Config::load_from_file(const std::filesystem::path& path) {
    std::ifstream stream(path);
    if (!stream.is_open()) {
        throw std::runtime_error("Failed to open configuration file: " + path.string());
    }
    const nlohmann::json doc = nlohmann::json::parse(stream, nullptr, true, true);

    Config cfg{};

    if (!doc.contains("level")) {
        throw std::runtime_error("Configuration missing required field: level");
    }

    const auto& level_node = doc.at("level");
    if (level_node.is_string()) {
        cfg.threshold = Logger::parse_level(level_node.get<std::string>());
        cfg.tag_level = true;
    } else if (level_node.is_object()) {
        if (!level_node.contains("threshold")) {
            throw std::runtime_error("level.threshold is required when level is an object");
        }
        cfg.threshold = Logger::parse_level(level_node.at("threshold").get<std::string>());
        if (level_node.contains("tag")) {
            cfg.tag_level = level_node.at("tag").get<bool>();
        }
    } else {
        throw std::runtime_error("level must be a string or object");
    }

    if (doc.contains("timestamp")) {
        cfg.timestamp = doc.at("timestamp").get<bool>();
    }
    if (doc.contains("file_location")) {
        if (doc.at("file_location").is_null()) {
            cfg.file_location.reset();
        } else {
            cfg.file_location = doc.at("file_location").get<std::string>();
        }
    }
    if (doc.contains("network_file_location")) {
        if (doc.at("network_file_location").is_null()) {
            cfg.network_file_location.reset();
        } else {
            cfg.network_file_location = doc.at("network_file_location").get<std::string>();
        }
    }

    return cfg;
}

inline void Logger::configure(const Config& config) {
    std::lock_guard lock(mutex_);
    config_ = config;
    configured_ = true;
}

inline void Logger::configure_from_file(const std::filesystem::path& path) {
    configure(Config::load_from_file(path));
}

inline bool Logger::should_emit(Level level) {
    return level_rank(level) >= level_rank(config_.threshold);
}

inline std::string Logger::level_to_string(Level level) {
    switch (level) {
        case Level::TRACE: return "TRACE";
        case Level::DEBUG: return "DEBUG";
        case Level::INFO: return "INFO";
        case Level::WARN: return "WARN";
        case Level::ERROR: return "ERROR";
        case Level::FATAL: return "FATAL";
    }
    return "INFO";
}

inline Level Logger::parse_level(std::string_view name) {
    std::string lower(name);
    for (auto& ch : lower) {
        ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
    }
    if (lower == "trace") return Level::TRACE;
    if (lower == "debug") return Level::DEBUG;
    if (lower == "info") return Level::INFO;
    if (lower == "warn" || lower == "warning") return Level::WARN;
    if (lower == "error") return Level::ERROR;
    if (lower == "fatal" || lower == "critical") return Level::FATAL;
    throw std::runtime_error("Unknown log level: " + std::string(name));
}

inline std::string Logger::build_prefix(Level level) {
    std::ostringstream prefix;
    bool first = true;
    if (config_.timestamp) {
        const auto now = std::chrono::system_clock::now();
        const auto tt = std::chrono::system_clock::to_time_t(now);
        std::tm tm{};
    #if defined(_WIN32)
        localtime_s(&tm, &tt);
    #else
        localtime_r(&tt, &tm);
    #endif
        prefix << std::put_time(&tm, "%Y-%m-%dT%H:%M:%S");
        first = false;
    }
    if (config_.tag_level) {
        if (!first) {
            prefix << ' ';
        }
        prefix << level_to_string(level);
        first = false;
    }
    if (!first) {
        prefix << ' ';
    }
    return prefix.str();
}

inline void Logger::emit(Level level, std::string_view message) {
    std::lock_guard lock(mutex_);
    if (!configured_) {
        throw std::runtime_error("Logger not configured. Call configure() first.");
    }
    if (!should_emit(level)) {
        return;
    }

    const std::string prefix = build_prefix(level);
    const std::string payload = prefix + std::string(message);

    std::cout << payload << std::endl;

    write_to_file(config_.file_location, payload);
    if (config_.network_file_location) {
        if (config_.network_file_location->rfind("tcp://", 0) == 0 ||
            config_.network_file_location->rfind("udp://", 0) == 0) {
            write_to_socket(*config_.network_file_location, payload);
        } else {
            write_to_file(config_.network_file_location, payload);
        }
    }
}

inline void Logger::write_to_file(const std::optional<std::string>& path, std::string_view payload) {
    if (!path || path->empty()) {
        return;
    }
    std::filesystem::path target(*path);
    if (!target.parent_path().empty()) {
        std::filesystem::create_directories(target.parent_path());
    }
    std::ofstream out(target, std::ios::app);
    if (!out.is_open()) {
        throw std::runtime_error("Failed to open log file: " + target.string());
    }
    out << payload << '
';
}

inline void Logger::write_to_socket(std::string_view target, std::string_view payload) {
    // TODO: Implement real socket writer. For now, fall back to treating the target as a file path.
    write_to_file(std::optional<std::string>{std::string(target)}, payload);
}

} // namespace liblogit

#define LOG(level) ::liblogit::LogMessage(::liblogit::Level::level)

#endif // LIBLOGIT_H
