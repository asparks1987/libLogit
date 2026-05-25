package dev.liblogit;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

public final class Logit {
    public enum Level {
        TRACE(0), DEBUG(1), INFO(2), WARN(3), ERROR(4), FATAL(5);

        final int rank;

        Level(int rank) {
            this.rank = rank;
        }
    }

    public String name = "default";
    public Level level = Level.INFO;
    public boolean timestamp = true;
    public boolean tagLevel = true;
    public String format = "text";
    public Map<String, Object> metadata = new LinkedHashMap<>();
    public boolean enabled = true;

    private String localPath;
    private String remotePath;
    private final List<String> sinks = new ArrayList<>();

    public Logit() {
        sinks.add("console");
    }

    public Logit(Map<String, Object> structure) {
        this();
        if (structure.containsKey("name")) {
            name = String.valueOf(structure.get("name"));
        }
        if (structure.containsKey("level")) {
            level = parseLevel(String.valueOf(structure.get("level")));
        }
        if (structure.containsKey("timestamp")) {
            timestamp = Boolean.TRUE.equals(structure.get("timestamp"));
        }
        if (structure.containsKey("tag_level")) {
            tagLevel = Boolean.TRUE.equals(structure.get("tag_level"));
        }
        if (structure.containsKey("format")) {
            format = String.valueOf(structure.get("format"));
        }
        if (structure.containsKey("metadata") && structure.get("metadata") instanceof Map<?, ?> rawMetadata) {
            metadata = new LinkedHashMap<>();
            for (Map.Entry<?, ?> entry : rawMetadata.entrySet()) {
                metadata.put(String.valueOf(entry.getKey()), entry.getValue());
            }
        }
        Object path = firstPresent(structure, "localPath", "local_path", "path", "file_location");
        if (path != null) {
            setLocalPath(String.valueOf(path));
        }
        Object remote = firstPresent(structure, "remotePath", "remote_path", "network_path", "network_file_location");
        if (remote != null) {
            setRemotePath(String.valueOf(remote));
        }
        if (structure.containsKey("sinks") && structure.get("sinks") instanceof List<?> rawSinks) {
            sinks.clear();
            for (Object sink : rawSinks) {
                addSink(String.valueOf(sink));
            }
        }
    }

    public String getLocalPath() {
        return localPath;
    }

    public void setLocalPath(String value) {
        localPath = value;
        setSink("file", value != null && !value.isBlank());
    }

    public String getRemotePath() {
        return remotePath;
    }

    public void setRemotePath(String value) {
        remotePath = value;
        setSink("network", value != null && !value.isBlank());
    }

    public List<String> getSinks() {
        return List.copyOf(sinks);
    }

    public void setSinks(List<String> values) {
        sinks.clear();
        for (String sink : values) {
            addSink(sink);
        }
    }

    public LogBuilder at(Level eventLevel) {
        return new LogBuilder(this, eventLevel);
    }

    public LogBuilder at(String eventLevel) {
        return at(parseLevel(eventLevel));
    }

    public void log(Level eventLevel, Object message) {
        if (!enabled || eventLevel.rank < level.rank) {
            return;
        }

        String rendered = render(eventLevel, stringify(message));
        if (sinks.contains("console")) {
            System.err.println(rendered);
        }
        if (sinks.contains("file") && localPath != null && !localPath.isBlank()) {
            appendLine(localPath, rendered);
        }
        if (sinks.contains("network") && remotePath != null && !remotePath.isBlank() && !looksLikeSocket(remotePath)) {
            appendLine(remotePath, rendered);
        }
    }

    public void log(String eventLevel, Object message) {
        log(parseLevel(eventLevel), message);
    }

    public static Map<String, Logit> loadLogits(String configPath) throws IOException {
        Object parsed = parseJson(Files.readString(Path.of(configPath)));
        if (!(parsed instanceof Map<?, ?> rawRoot)) {
            throw new IllegalArgumentException("Configuration root must be an object");
        }
        Map<String, Object> root = castMap(rawRoot);
        if (!root.containsKey("logits")) {
            Map<String, Object> structure = new LinkedHashMap<>();
            structure.put("name", "default");
            Object levelNode = root.get("level");
            if (levelNode instanceof String textLevel) {
                structure.put("level", textLevel);
            } else if (levelNode instanceof Map<?, ?> mapLevel) {
                Map<String, Object> levelMap = castMap(mapLevel);
                structure.put("level", levelMap.get("threshold"));
                structure.put("tag_level", levelMap.getOrDefault("tag", true));
            }
            structure.put("timestamp", root.getOrDefault("timestamp", true));
            structure.put("path", root.get("file_location"));
            structure.put("remotePath", root.get("network_file_location"));
            return Map.of("default", new Logit(structure));
        }

        Map<String, Object> defaults = root.get("defaults") instanceof Map<?, ?> rawDefaults
            ? castMap(rawDefaults)
            : Map.of();
        Map<String, Object> logitMap = castMap((Map<?, ?>) root.get("logits"));
        Map<String, Logit> result = new LinkedHashMap<>();
        for (Map.Entry<String, Object> entry : logitMap.entrySet()) {
            Map<String, Object> merged = new LinkedHashMap<>(defaults);
            merged.putAll(castMap((Map<?, ?>) entry.getValue()));
            merged.putIfAbsent("name", entry.getKey());
            result.put(String.valueOf(merged.get("name")), new Logit(merged));
        }
        return result;
    }

    public static Object parseJson(String text) {
        return new JsonParser(text).parse();
    }

    private String render(Level eventLevel, String message) {
        if ("json".equalsIgnoreCase(format)) {
            Map<String, Object> event = new TreeMap<>();
            event.put("level", canonicalLevel(eventLevel));
            event.put("logger", name);
            event.put("message", message);
            if (!metadata.isEmpty()) {
                event.put("metadata", metadata);
            }
            if (timestamp) {
                event.put("timestamp", OffsetDateTime.now().toString());
            }
            return toJson(event);
        }

        List<String> parts = new ArrayList<>();
        if (timestamp) {
            parts.add(OffsetDateTime.now().toString());
        }
        if (tagLevel) {
            parts.add(renderedLevel(eventLevel));
        }
        parts.add(message);
        return String.join(" ", parts);
    }

    private static void appendLine(String target, String line) {
        try {
            Path file = Path.of(target);
            Path parent = file.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            Files.writeString(file, line + System.lineSeparator(), StandardCharsets.UTF_8,
                Files.exists(file) ? java.nio.file.StandardOpenOption.APPEND : java.nio.file.StandardOpenOption.CREATE);
        } catch (IOException exc) {
            System.err.println("Unable to write log file " + target + ": " + exc.getMessage());
        }
    }

    private static void setSink(List<String> sinks, String sink, boolean enabled) {
        if (enabled && !sinks.contains(sink)) {
            sinks.add(sink);
        }
        if (!enabled) {
            sinks.remove(sink);
        }
    }

    private void setSink(String sink, boolean enabled) {
        setSink(sinks, sink, enabled);
    }

    private void addSink(String sink) {
        String normalized = sink.toLowerCase(Locale.ROOT);
        if (!List.of("console", "file", "network").contains(normalized)) {
            throw new IllegalArgumentException("Unknown sink: " + sink);
        }
        if (!sinks.contains(normalized)) {
            sinks.add(normalized);
        }
    }

    private static boolean looksLikeSocket(String value) {
        return value.toLowerCase(Locale.ROOT).startsWith("tcp://")
            || value.toLowerCase(Locale.ROOT).startsWith("udp://");
    }

    private static Object firstPresent(Map<String, Object> map, String... keys) {
        for (String key : keys) {
            if (map.containsKey(key) && map.get(key) != null) {
                return map.get(key);
            }
        }
        return null;
    }

    private static String stringify(Object value) {
        if (value == null) {
            return "null";
        }
        if (value instanceof String text) {
            return text;
        }
        if (value instanceof Map<?, ?> map) {
            return toJson(castMap(map));
        }
        return String.valueOf(value);
    }

    public static Level parseLevel(String value) {
        return switch (value.trim().toLowerCase(Locale.ROOT)) {
            case "trace" -> Level.TRACE;
            case "debug" -> Level.DEBUG;
            case "info" -> Level.INFO;
            case "warn", "warning" -> Level.WARN;
            case "error" -> Level.ERROR;
            case "fatal" -> Level.FATAL;
            default -> throw new IllegalArgumentException("Unknown level: " + value);
        };
    }

    private static String renderedLevel(Level value) {
        return switch (value) {
            case TRACE -> "TRACE";
            case DEBUG -> "DEBUG";
            case INFO -> "INFO";
            case WARN -> "WARNING";
            case ERROR -> "ERROR";
            case FATAL -> "CRITICAL";
        };
    }

    private static String canonicalLevel(Level value) {
        return value.name().toLowerCase(Locale.ROOT);
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> castMap(Map<?, ?> raw) {
        Map<String, Object> result = new LinkedHashMap<>();
        for (Map.Entry<?, ?> entry : raw.entrySet()) {
            result.put(String.valueOf(entry.getKey()), entry.getValue());
        }
        return result;
    }

    private static String toJson(Map<String, Object> map) {
        StringBuilder builder = new StringBuilder("{");
        boolean first = true;
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (!first) {
                builder.append(',');
            }
            first = false;
            builder.append('"').append(escape(entry.getKey())).append('"').append(':');
            Object value = entry.getValue();
            if (value instanceof Map<?, ?> nested) {
                builder.append(toJson(castMap(nested)));
            } else {
                builder.append('"').append(escape(String.valueOf(value))).append('"');
            }
        }
        builder.append('}');
        return builder.toString();
    }

    private static String escape(String value) {
        return value.replace("\\", "\\\\").replace("\"", "\\\"");
    }

    public static final class LogBuilder {
        private final Logit logit;
        private final Level level;
        private final StringBuilder buffer = new StringBuilder();
        private boolean committed;

        LogBuilder(Logit logit, Level level) {
            this.logit = logit;
            this.level = level;
        }

        public LogBuilder append(Object value) {
            if (!committed) {
                buffer.append(value);
            }
            return this;
        }

        public LogBuilder commit() {
            if (!committed && buffer.length() > 0) {
                committed = true;
                logit.log(level, buffer.toString());
            }
            return this;
        }
    }

    private static final class JsonParser {
        private final String text;
        private int index;

        JsonParser(String text) {
            this.text = text;
        }

        Object parse() {
            skipWhitespace();
            Object value = parseValue();
            skipWhitespace();
            if (index != text.length()) {
                throw new IllegalArgumentException("Unexpected trailing JSON");
            }
            return value;
        }

        private Object parseValue() {
            skipWhitespace();
            if (peek('{')) {
                return parseObject();
            }
            if (peek('[')) {
                return parseArray();
            }
            if (peek('"')) {
                return parseString();
            }
            if (text.startsWith("true", index)) {
                index += 4;
                return true;
            }
            if (text.startsWith("false", index)) {
                index += 5;
                return false;
            }
            if (text.startsWith("null", index)) {
                index += 4;
                return null;
            }
            throw new IllegalArgumentException("Unsupported JSON token at " + index);
        }

        private Map<String, Object> parseObject() {
            Map<String, Object> result = new LinkedHashMap<>();
            expect('{');
            skipWhitespace();
            if (peek('}')) {
                index++;
                return result;
            }
            while (true) {
                String key = parseString();
                skipWhitespace();
                expect(':');
                result.put(key, parseValue());
                skipWhitespace();
                if (peek('}')) {
                    index++;
                    return result;
                }
                expect(',');
                skipWhitespace();
            }
        }

        private List<Object> parseArray() {
            List<Object> result = new ArrayList<>();
            expect('[');
            skipWhitespace();
            if (peek(']')) {
                index++;
                return result;
            }
            while (true) {
                result.add(parseValue());
                skipWhitespace();
                if (peek(']')) {
                    index++;
                    return result;
                }
                expect(',');
                skipWhitespace();
            }
        }

        private String parseString() {
            expect('"');
            StringBuilder builder = new StringBuilder();
            while (index < text.length()) {
                char current = text.charAt(index++);
                if (current == '"') {
                    return builder.toString();
                }
                if (current == '\\') {
                    char escaped = text.charAt(index++);
                    builder.append(switch (escaped) {
                        case '"', '\\', '/' -> escaped;
                        case 'b' -> '\b';
                        case 'f' -> '\f';
                        case 'n' -> '\n';
                        case 'r' -> '\r';
                        case 't' -> '\t';
                        default -> throw new IllegalArgumentException("Unsupported escape: " + escaped);
                    });
                } else {
                    builder.append(current);
                }
            }
            throw new IllegalArgumentException("Unterminated string");
        }

        private void expect(char value) {
            if (!peek(value)) {
                throw new IllegalArgumentException("Expected '" + value + "' at " + index);
            }
            index++;
        }

        private boolean peek(char value) {
            return index < text.length() && text.charAt(index) == value;
        }

        private void skipWhitespace() {
            while (index < text.length() && Character.isWhitespace(text.charAt(index))) {
                index++;
            }
        }
    }
}
