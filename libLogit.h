#ifndef LIBLOGIT_H
#define LIBLOGIT_H

#include <chrono>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <mutex>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <cctype>

// Requires nlohmann/json.hpp (https://github.com/nlohmann/json)
#include <nlohmann/json.hpp>

namespace liblogit {

/** Severity levels exposed to consumers of the logging API. */
enum class Level { TRACE, DEBUG, INFO, WARN, ERROR, FATAL };

class LOGIT;

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
    friend class LOGIT;

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
    LogMessage(const LOGIT& logit, Level level) : target_(&logit), level_(level) {}
    ~LogMessage();

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

    void commit();

private:
    void flush();

    const LOGIT* target_ = nullptr;
    Level level_;
    std::ostringstream stream_;
    bool committed_ = false;
};

/** User-owned logger object for v1 alpha direct-instantiation workflows. */
class LOGIT {
public:
    std::string name = "default";
    std::optional<std::string> path;
    std::optional<std::string> localPath;
    std::optional<std::string> remotePath;
    Level level = Level::INFO;
    bool enabled = true;
    bool timestamp = true;
    bool tagLevel = true;
    std::string format = "text";
    std::map<std::string, std::string> metadata;
    std::vector<std::string> sinks = {"console"};

    LOGIT() = default;
    explicit LOGIT(const nlohmann::json& structure) { apply_structure(structure); }

    LogMessage operator()(Level event_level) const { return LogMessage(*this, event_level); }
    LogMessage at(Level event_level) const { return LogMessage(*this, event_level); }
    void log(Level event_level, std::string_view message) const { emit(event_level, message); }
    void emit(Level event_level, std::string_view message) const;

    static std::map<std::string, LOGIT> load_from_file(const std::filesystem::path& config_path);
    static Level parse_level(std::string_view name) { return Logger::parse_level(name); }

private:
    bool sinks_authoritative_ = false;

    void apply_structure(const nlohmann::json& structure);
    std::optional<std::string> effective_local_path() const;
    bool has_sink(std::string_view sink) const;
    bool should_emit(Level event_level) const;
    std::string render(Level event_level, std::string_view message) const;
    static std::string rendered_level(Level level);
    static std::string canonical_level(Level level);
    static std::string lowercase(std::string_view value);
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
        case Level::WARN: return "WARNING";
        case Level::ERROR: return "ERROR";
        case Level::FATAL: return "CRITICAL";
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
    out << payload << '\n';
}

inline void Logger::write_to_socket(std::string_view target, std::string_view payload) {
    // TODO: Implement real socket writer. For now, fall back to treating the target as a file path.
    write_to_file(std::optional<std::string>{std::string(target)}, payload);
}

inline LogMessage::~LogMessage() {
    flush();
}

inline void LogMessage::flush() {
    commit();
}

inline void LogMessage::commit() {
    if (committed_) {
        return;
    }
    committed_ = true;
    const std::string payload = stream_.str();
    if (payload.empty()) {
        return;
    }
    if (target_ != nullptr) {
        target_->emit(level_, payload);
        return;
    }
    Logger::emit(level_, payload);
}

inline void LOGIT::apply_structure(const nlohmann::json& structure) {
    if (!structure.is_object()) {
        throw std::runtime_error("LOGIT structure must be a JSON object");
    }

    if (structure.contains("name") && !structure.at("name").is_null()) {
        name = structure.at("name").get<std::string>();
    }

    if (structure.contains("level") && !structure.at("level").is_null()) {
        const auto& node = structure.at("level");
        if (node.is_string()) {
            level = parse_level(node.get<std::string>());
        } else if (node.is_object()) {
            if (node.contains("threshold")) {
                level = parse_level(node.at("threshold").get<std::string>());
            }
            if (node.contains("tag")) {
                tagLevel = node.at("tag").get<bool>();
            }
        } else {
            throw std::runtime_error("LOGIT level must be a string or object");
        }
    }

    if (structure.contains("timestamp") && !structure.at("timestamp").is_null()) {
        timestamp = structure.at("timestamp").get<bool>();
    }
    if (structure.contains("tag_level") && !structure.at("tag_level").is_null()) {
        tagLevel = structure.at("tag_level").get<bool>();
    }
    if (structure.contains("tagLevel") && !structure.at("tagLevel").is_null()) {
        tagLevel = structure.at("tagLevel").get<bool>();
    }
    if (structure.contains("enabled") && !structure.at("enabled").is_null()) {
        enabled = structure.at("enabled").get<bool>();
    }
    if (structure.contains("format") && !structure.at("format").is_null()) {
        format = structure.at("format").get<std::string>();
    }

    const char* local_keys[] = {"localPath", "local_path", "path", "file_location"};
    for (const char* key : local_keys) {
        if (structure.contains(key) && !structure.at(key).is_null()) {
            localPath = structure.at(key).get<std::string>();
            path = localPath;
            break;
        }
    }

    const char* remote_keys[] = {"remotePath", "remote_path", "network_path", "network_file_location"};
    for (const char* key : remote_keys) {
        if (structure.contains(key) && !structure.at(key).is_null()) {
            remotePath = structure.at(key).get<std::string>();
            break;
        }
    }

    if (structure.contains("metadata") && structure.at("metadata").is_object()) {
        metadata.clear();
        for (const auto& item : structure.at("metadata").items()) {
            metadata[item.key()] = item.value().is_string()
                ? item.value().get<std::string>()
                : item.value().dump();
        }
    }

    if (structure.contains("sinks") && structure.at("sinks").is_array()) {
        sinks_authoritative_ = true;
        sinks.clear();
        for (const auto& sink : structure.at("sinks")) {
            sinks.push_back(lowercase(sink.get<std::string>()));
        }
    }
}

inline std::map<std::string, LOGIT> LOGIT::load_from_file(const std::filesystem::path& config_path) {
    std::ifstream stream(config_path);
    if (!stream.is_open()) {
        throw std::runtime_error("Failed to open configuration file: " + config_path.string());
    }
    const nlohmann::json doc = nlohmann::json::parse(stream, nullptr, true, true);
    std::map<std::string, LOGIT> result;

    if (!doc.contains("logits")) {
        nlohmann::json structure = nlohmann::json::object();
        structure["name"] = "default";
        if (doc.contains("level")) {
            structure["level"] = doc.at("level");
        }
        if (doc.contains("timestamp")) {
            structure["timestamp"] = doc.at("timestamp");
        }
        if (doc.contains("file_location")) {
            structure["path"] = doc.at("file_location");
        }
        if (doc.contains("network_file_location")) {
            structure["remotePath"] = doc.at("network_file_location");
        }
        LOGIT logit(structure);
        result.emplace(logit.name, std::move(logit));
        return result;
    }

    const nlohmann::json defaults = doc.value("defaults", nlohmann::json::object());
    for (const auto& item : doc.at("logits").items()) {
        nlohmann::json merged = defaults;
        for (const auto& field : item.value().items()) {
            merged[field.key()] = field.value();
        }
        if (!merged.contains("name")) {
            merged["name"] = item.key();
        }
        LOGIT logit(merged);
        result.emplace(logit.name, std::move(logit));
    }
    return result;
}

inline void LOGIT::emit(Level event_level, std::string_view message) const {
    if (!should_emit(event_level)) {
        return;
    }

    const std::string payload = render(event_level, message);
    if (has_sink("console")) {
        std::cerr << payload << std::endl;
    }

    const std::optional<std::string> local = effective_local_path();
    if ((has_sink("file") || (!sinks_authoritative_ && local.has_value())) && local && !local->empty()) {
        try {
            Logger::write_to_file(local, payload);
        } catch (const std::exception& exc) {
            std::cerr << "Unable to write log file " << *local << ": " << exc.what() << std::endl;
        }
    }

    if ((has_sink("network") || (!sinks_authoritative_ && remotePath.has_value())) &&
        remotePath && !remotePath->empty() &&
        remotePath->rfind("tcp://", 0) != 0 &&
        remotePath->rfind("udp://", 0) != 0) {
        try {
            Logger::write_to_file(remotePath, payload);
        } catch (const std::exception& exc) {
            std::cerr << "Unable to write remote log file " << *remotePath << ": " << exc.what() << std::endl;
        }
    }
}

inline std::optional<std::string> LOGIT::effective_local_path() const {
    if (localPath && !localPath->empty()) {
        return localPath;
    }
    if (path && !path->empty()) {
        return path;
    }
    return std::nullopt;
}

inline bool LOGIT::has_sink(std::string_view sink) const {
    const std::string wanted = lowercase(sink);
    for (const std::string& configured : sinks) {
        if (lowercase(configured) == wanted) {
            return true;
        }
    }
    return false;
}

inline bool LOGIT::should_emit(Level event_level) const {
    return enabled && level_rank(event_level) >= level_rank(level);
}

inline std::string LOGIT::render(Level event_level, std::string_view message) const {
    if (lowercase(format) == "json") {
        nlohmann::json event;
        event["level"] = canonical_level(event_level);
        event["logger"] = name;
        event["message"] = std::string(message);
        if (!metadata.empty()) {
            event["metadata"] = metadata;
        }
        if (timestamp) {
            const auto now = std::chrono::system_clock::now();
            const auto tt = std::chrono::system_clock::to_time_t(now);
            std::tm tm{};
        #if defined(_WIN32)
            localtime_s(&tm, &tt);
        #else
            localtime_r(&tt, &tm);
        #endif
            std::ostringstream value;
            value << std::put_time(&tm, "%Y-%m-%dT%H:%M:%S");
            event["timestamp"] = value.str();
        }
        return event.dump();
    }

    std::ostringstream output;
    bool first = true;
    if (timestamp) {
        const auto now = std::chrono::system_clock::now();
        const auto tt = std::chrono::system_clock::to_time_t(now);
        std::tm tm{};
    #if defined(_WIN32)
        localtime_s(&tm, &tt);
    #else
        localtime_r(&tt, &tm);
    #endif
        output << std::put_time(&tm, "%Y-%m-%dT%H:%M:%S");
        first = false;
    }
    if (tagLevel) {
        if (!first) {
            output << ' ';
        }
        output << rendered_level(event_level);
        first = false;
    }
    if (!first) {
        output << ' ';
    }
    output << message;
    return output.str();
}

inline std::string LOGIT::rendered_level(Level level) {
    return Logger::level_to_string(level);
}

inline std::string LOGIT::canonical_level(Level level) {
    switch (level) {
        case Level::TRACE: return "trace";
        case Level::DEBUG: return "debug";
        case Level::INFO: return "info";
        case Level::WARN: return "warn";
        case Level::ERROR: return "error";
        case Level::FATAL: return "fatal";
    }
    return "info";
}

inline std::string LOGIT::lowercase(std::string_view value) {
    std::string lower(value);
    for (auto& ch : lower) {
        ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
    }
    return lower;
}

} // namespace liblogit

#define LOG(level) ::liblogit::LogMessage(::liblogit::Level::level)

#endif // LIBLOGIT_H
