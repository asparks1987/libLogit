/**
 * Minimal JVM binding around the shared JSON configuration.
 */
package dev.liblogit;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.logging.ConsoleHandler;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/** Utility class housing static entry points for libLogit. */
public final class LibLogIt {
    private static final Logger LOGGER = Logger.getLogger("liblogit");
    private static final DateTimeFormatter ISO = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")
            .withZone(ZoneId.systemDefault());

    private static Config config;

    private LibLogIt() {
    }

    /** Initialise logging from a JSON configuration file. */
    public static synchronized void initFromConfig(String path) throws IOException {
        String json = Files.readString(Path.of(path));
        config = Config.fromJson(json);
        configureLogger();
    }

    /** Entry point mirroring the C++ LOG(level) helper. */
    public static synchronized LogBuilder LOG(String level) {
        ensureConfigured();
        return new LogBuilder(level);
    }

    static synchronized void log(String level, String message) {
        ensureConfigured();
        Level javaLevel = toJavaLevel(level);
        if (javaLevel.intValue() < toJavaLevel(config.threshold).intValue()) {
            return;
        }
        LOGGER.log(javaLevel, message);
    }

    private static void ensureConfigured() {
        if (config == null) {
            throw new IllegalStateException("LibLogIt not configured. Call initFromConfig first.");
        }
    }

    private static void configureLogger() throws IOException {
        LOGGER.setUseParentHandlers(false);
        LOGGER.setLevel(toJavaLevel(config.threshold));
        for (Handler handler : LOGGER.getHandlers()) {
            LOGGER.removeHandler(handler);
        }

        Formatter formatter = new Formatter() {
            @Override
            public String format(LogRecord record) {
                StringBuilder builder = new StringBuilder();
                if (config.timestamp) {
                    builder.append(ISO.format(Instant.ofEpochMilli(record.getMillis()))).append(' ');
                }
                if (config.tagLevel) {
                    builder.append(record.getLevel().getName()).append(' ');
                }
                builder.append(formatMessage(record));
                if (record.getThrown() != null) {
                    StringWriter sw = new StringWriter();
                    record.getThrown().printStackTrace(new PrintWriter(sw));
                    builder.append(System.lineSeparator()).append(sw);
                }
                builder.append(System.lineSeparator());
                return builder.toString();
            }
        };

        ConsoleHandler console = new ConsoleHandler();
        console.setFormatter(formatter);
        console.setLevel(toJavaLevel(config.threshold));
        LOGGER.addHandler(console);

        if (config.fileLocation != null && !config.fileLocation.isBlank()) {
            Path file = Path.of(config.fileLocation);
            Path parent = file.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            FileHandler fileHandler = new FileHandler(file.toString(), true);
            fileHandler.setFormatter(formatter);
            fileHandler.setLevel(toJavaLevel(config.threshold));
            LOGGER.addHandler(fileHandler);
        }

        if (config.networkFileLocation != null && !config.networkFileLocation.isBlank()) {
            Path file = Path.of(config.networkFileLocation);
            Path parent = file.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            FileHandler networkHandler = new FileHandler(file.toString(), true);
            networkHandler.setFormatter(formatter);
            networkHandler.setLevel(toJavaLevel(config.threshold));
            LOGGER.addHandler(networkHandler);
        }
    }

    private static Level toJavaLevel(String value) {
        String lower = value.toLowerCase(Locale.ROOT);
        switch (lower) {
            case "trace":
                return Level.FINEST;
            case "debug":
                return Level.FINE;
            case "info":
                return Level.INFO;
            case "warn":
            case "warning":
                return Level.WARNING;
            case "error":
            case "fatal":
                return Level.SEVERE;
            default:
                throw new IllegalArgumentException("Unknown level: " + value);
        }
    }

    /** Streaming builder that buffers fragments before committing a message. */
    public static final class LogBuilder {
        private final String level;
        private final StringBuilder buffer = new StringBuilder();
        private boolean committed;

        private LogBuilder(String level) {
            this.level = level;
        }

        public LogBuilder append(Object value) {
            if (!committed) {
                buffer.append(stringify(value));
            }
            return this;
        }

        public void commit() {
            if (!committed) {
                committed = true;
                LibLogIt.log(level, buffer.toString());
            }
        }

        @Override
        protected void finalize() throws Throwable {
            try {
                commit();
            } finally {
                super.finalize();
            }
        }
    }

    private static String stringify(Object value) {
        if (value instanceof String) {
            return (String) value;
        }
        if (value instanceof Map) {
            return value.toString();
        }
        return Objects.toString(value);
    }

    /** Value object representing the subset of configuration fields consumed by the JVM binding. */
    private static final class Config {
        final String threshold;
        final boolean tagLevel;
        final boolean timestamp;
        final String fileLocation;
        final String networkFileLocation;

        Config(String threshold, boolean tagLevel, boolean timestamp, String fileLocation, String networkFileLocation) {
            this.threshold = threshold;
            this.tagLevel = tagLevel;
            this.timestamp = timestamp;
            this.fileLocation = fileLocation;
            this.networkFileLocation = networkFileLocation;
        }

        static Config fromJson(String json) {
            SimpleJsonParser parser = new SimpleJsonParser(json);
            Object root = parser.parse();
            if (!(root instanceof Map)) {
                throw new IllegalArgumentException("Configuration root must be an object");
            }
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) root;
            if (!map.containsKey("level")) {
                throw new IllegalArgumentException("Missing required field: level");
            }
            if (map.keySet().size() > 4) {
                throw new IllegalArgumentException("Only level, timestamp, file_location, network_file_location are allowed");
            }
            Object levelNode = map.get("level");
            String threshold;
            boolean tag = true;
            if (levelNode instanceof String) {
                threshold = normaliseLevel((String) levelNode);
            } else if (levelNode instanceof Map) {
                @SuppressWarnings("unchecked")
                Map<String, Object> inner = (Map<String, Object>) levelNode;
                Object rawThreshold = inner.get("threshold");
                if (!(rawThreshold instanceof String)) {
                    throw new IllegalArgumentException("level.threshold must be a string");
                }
                threshold = normaliseLevel((String) rawThreshold);
                if (inner.containsKey("tag")) {
                    Object tagNode = inner.get("tag");
                    if (tagNode instanceof Boolean) {
                        tag = (Boolean) tagNode;
                    } else {
                        throw new IllegalArgumentException("level.tag must be boolean");
                    }
                }
            } else {
                throw new IllegalArgumentException("level must be string or object");
            }

            boolean timestamp = true;
            if (map.containsKey("timestamp")) {
                Object timestampNode = map.get("timestamp");
                if (timestampNode instanceof Boolean) {
                    timestamp = (Boolean) timestampNode;
                } else {
                    throw new IllegalArgumentException("timestamp must be boolean");
                }
            }

            String fileLocation = null;
            if (map.containsKey("file_location")) {
                Object fileNode = map.get("file_location");
                if (fileNode == SimpleJsonParser.NULL || fileNode == null) {
                    fileLocation = null;
                } else if (fileNode instanceof String) {
                    fileLocation = (String) fileNode;
                } else {
                    throw new IllegalArgumentException("file_location must be string or null");
                }
            }

            String networkLocation = null;
            if (map.containsKey("network_file_location")) {
                Object networkNode = map.get("network_file_location");
                if (networkNode == SimpleJsonParser.NULL || networkNode == null) {
                    networkLocation = null;
                } else if (networkNode instanceof String) {
                    networkLocation = (String) networkNode;
                } else {
                    throw new IllegalArgumentException("network_file_location must be string or null");
                }
            }

            return new Config(threshold, tag, timestamp, fileLocation, networkLocation);
        }

        private static String normaliseLevel(String raw) {
            String lower = raw.toLowerCase(Locale.ROOT);
            if ("warning".equals(lower)) {
                lower = "warn";
            }
            switch (lower) {
                case "trace":
                case "debug":
                case "info":
                case "warn":
                case "error":
                case "fatal":
                    return lower;
                default:
                    throw new IllegalArgumentException("Unknown level: " + raw);
            }
        }
    }

    /** Minimal JSON parser to avoid external dependencies inside the sample binding. */
    private static final class SimpleJsonParser {
        static final Object NULL = new Object();

        private final String text;
        private int index;

        SimpleJsonParser(String text) {
            this.text = text;
        }

        Object parse() {
            skipWhitespace();
            Object value = parseValue();
            skipWhitespace();
            if (index != text.length()) {
                throw new IllegalArgumentException("Unexpected trailing content at position " + index);
            }
            return value;
        }

        private Object parseValue() {
            skipWhitespace();
            if (index >= text.length()) {
                throw new IllegalArgumentException("Unexpected end of input");
            }
            char ch = text.charAt(index);
            if (ch == '{') {
                return parseObject();
            }
            if (ch == '"') {
                return parseString();
            }
            if (text.startsWith("true", index)) {
                index += 4;
                return Boolean.TRUE;
            }
            if (text.startsWith("false", index)) {
                index += 5;
                return Boolean.FALSE;
            }
            if (text.startsWith("null", index)) {
                index += 4;
                return NULL;
            }
            throw new IllegalArgumentException("Unsupported token at position " + index);
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
                skipWhitespace();
                Object value = parseValue();
                result.put(key, value);
                skipWhitespace();
                if (peek('}')) {
                    index++;
                    break;
                }
                expect(',');
                skipWhitespace();
            }
            return result;
        }

        private String parseString() {
            expect('"');
            StringBuilder builder = new StringBuilder();
            while (index < text.length()) {
                char ch = text.charAt(index++);
                if (ch == '"') {
                    return builder.toString();
                }
                if (ch == '') {
                    if (index >= text.length()) {
                        throw new IllegalArgumentException("Invalid escape sequence");
                    }
                    char esc = text.charAt(index++);
                    switch (esc) {
                        case '"':
                            builder.append('"');
                            break;
                        case '':
                            builder.append('');
                            break;
                        case '/':
                            builder.append('/');
                            break;
                        case 'b':
                            builder.append('');
                            break;
                        case 'f':
                            builder.append('');
                            break;
                        case 'n':
                            builder.append('
');
                            break;
                        case 'r':
                            builder.append('
');
                            break;
                        case 't':
                            builder.append('	');
                            break;
                        case 'u':
                            if (index + 4 > text.length()) {
                                throw new IllegalArgumentException("Invalid unicode escape");
                            }
                            String hex = text.substring(index, index + 4);
                            index += 4;
                            builder.append((char) Integer.parseInt(hex, 16));
                            break;
                        default:
                            throw new IllegalArgumentException("Unsupported escape: \" + esc);
                    }
                } else {
                    builder.append(ch);
                }
            }
            throw new IllegalArgumentException("Unterminated string literal");
        }

        private void expect(char expected) {
            if (index >= text.length() || text.charAt(index) != expected) {
                throw new IllegalArgumentException("Expected '" + expected + "' at position " + index);
            }
            index++;
        }

        private boolean peek(char ch) {
            return index < text.length() && text.charAt(index) == ch;
        }

        private void skipWhitespace() {
            while (index < text.length() && Character.isWhitespace(text.charAt(index))) {
                index++;
            }
        }
    }
}
