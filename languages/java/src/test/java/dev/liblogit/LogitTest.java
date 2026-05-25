package dev.liblogit;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

public final class LogitTest {
    public static void main(String[] args) throws Exception {
        testBlankDeclaration();
        testConfigLoading();
        testJsonOutput();
        testConformanceFixtures();
        System.out.println("Java libLogit tests passed");
    }

    private static void testBlankDeclaration() throws Exception {
        Path dir = Files.createTempDirectory("liblogit-java-");
        Path logPath = dir.resolve("app.log");
        Logit logIT = new Logit();
        logIT.setLocalPath(logPath.toString());
        logIT.level = Logit.Level.DEBUG;
        logIT.timestamp = false;
        logIT.setSinks(List.of("file"));

        logIT.at(Logit.Level.DEBUG).append("hello").append(" world").commit();
        assertLines(logPath, "DEBUG hello world");
    }

    private static void testConfigLoading() throws Exception {
        Path dir = Files.createTempDirectory("liblogit-java-");
        Path logPath = dir.resolve("app.log");
        Path configPath = dir.resolve("logit.json");
        Files.writeString(configPath, """
            {
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
                  "path": "%s",
                  "level": "debug"
                }
              }
            }
            """.formatted(logPath.toString().replace("\\", "\\\\")));

        Map<String, Logit> logits = Logit.loadLogits(configPath.toString());
        logits.get("AppLog").at("debug").append("from config").commit();
        assertLines(logPath, "DEBUG from config");
    }

    private static void testJsonOutput() throws Exception {
        Path dir = Files.createTempDirectory("liblogit-java-");
        Path logPath = dir.resolve("audit.jsonl");
        Logit audit = new Logit(Map.of(
            "name", "AuditLog",
            "localPath", logPath.toString(),
            "level", "info",
            "timestamp", false,
            "format", "json",
            "sinks", List.of("file"),
            "metadata", Map.of("component", "audit")
        ));

        audit.log(Logit.Level.INFO, "user signed in");
        assertLines(logPath, "{\"level\":\"info\",\"logger\":\"AuditLog\",\"message\":\"user signed in\",\"metadata\":{\"component\":\"audit\"}}");
    }

    private static void assertLines(Path path, String... expected) throws Exception {
        List<String> actual = Files.readAllLines(path);
        if (!actual.equals(List.of(expected))) {
            throw new AssertionError("Unexpected contents for " + path + ": " + actual);
        }
    }

    private static void testConformanceFixtures() throws Exception {
        Path fixtureDir = Path.of("tests", "conformance", "fixtures");
        try (Stream<Path> paths = Files.list(fixtureDir)) {
            for (Path fixturePath : paths.filter(path -> path.toString().endsWith(".json")).sorted().toList()) {
                @SuppressWarnings("unchecked")
                Map<String, Object> fixture = (Map<String, Object>) Logit.parseJson(Files.readString(fixturePath));
                Path dir = Files.createTempDirectory("liblogit-java-");

                @SuppressWarnings("unchecked")
                Map<String, Object> config = deepCopyMap((Map<String, Object>) fixture.get("config"));
                @SuppressWarnings("unchecked")
                Map<String, Object> logits = (Map<String, Object>) config.get("logits");
                for (Object value : logits.values()) {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> logitConfig = (Map<String, Object>) value;
                    rewritePath(logitConfig, "path", dir);
                    rewritePath(logitConfig, "localPath", dir);
                    rewritePath(logitConfig, "file_location", dir);
                    rewritePath(logitConfig, "remotePath", dir);
                    rewritePath(logitConfig, "remote_path", dir);
                    rewritePath(logitConfig, "network_path", dir);
                    rewritePath(logitConfig, "network_file_location", dir);
                }

                Path configPath = dir.resolve("logit.json");
                Files.writeString(configPath, toJson(config));
                Map<String, Logit> loaded = Logit.loadLogits(configPath.toString());

                @SuppressWarnings("unchecked")
                List<Object> messages = (List<Object>) fixture.get("messages");
                for (Object rawMessage : messages) {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> message = (Map<String, Object>) rawMessage;
                    Logit.LogBuilder builder = loaded.get(String.valueOf(message.get("logger"))).at(String.valueOf(message.get("level")));
                    @SuppressWarnings("unchecked")
                    List<Object> fragments = (List<Object>) message.get("fragments");
                    for (Object fragment : fragments) {
                        builder.append(fragment);
                    }
                    builder.commit();
                }

                @SuppressWarnings("unchecked")
                Map<String, Object> expectedFiles = (Map<String, Object>) fixture.get("expected_files");
                for (Map.Entry<String, Object> expectedFile : expectedFiles.entrySet()) {
                    @SuppressWarnings("unchecked")
                    List<Object> expected = (List<Object>) expectedFile.getValue();
                    assertLines(dir.resolve(expectedFile.getKey()), expected.stream().map(String::valueOf).toArray(String[]::new));
                }
            }
        }
    }

    private static void rewritePath(Map<String, Object> config, String key, Path dir) {
        Object value = config.get(key);
        if (value instanceof String text && !text.isBlank()) {
            config.put(key, dir.resolve(text).toString());
        }
    }

    private static Map<String, Object> deepCopyMap(Map<String, Object> source) {
        Map<String, Object> result = new LinkedHashMap<>();
        for (Map.Entry<String, Object> entry : source.entrySet()) {
            Object value = entry.getValue();
            if (value instanceof Map<?, ?> nested) {
                @SuppressWarnings("unchecked")
                Map<String, Object> nestedMap = (Map<String, Object>) nested;
                result.put(entry.getKey(), deepCopyMap(nestedMap));
            } else if (value instanceof List<?> list) {
                result.put(entry.getKey(), new ArrayList<>(list));
            } else {
                result.put(entry.getKey(), value);
            }
        }
        return result;
    }

    private static String toJson(Object value) {
        if (value instanceof Map<?, ?> map) {
            StringBuilder builder = new StringBuilder("{");
            boolean first = true;
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                if (!first) builder.append(',');
                first = false;
                builder.append(toJson(String.valueOf(entry.getKey()))).append(':').append(toJson(entry.getValue()));
            }
            return builder.append('}').toString();
        }
        if (value instanceof List<?> list) {
            StringBuilder builder = new StringBuilder("[");
            boolean first = true;
            for (Object item : list) {
                if (!first) builder.append(',');
                first = false;
                builder.append(toJson(item));
            }
            return builder.append(']').toString();
        }
        if (value instanceof Boolean bool) {
            return bool.toString();
        }
        if (value == null) {
            return "null";
        }
        return "\"" + String.valueOf(value).replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }
}
