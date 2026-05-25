package dev.liblogit;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Compatibility facade for the original JVM-style static libLogit API.
 *
 * <p>The Alpha package surface is {@link Logit}; this class keeps older
 * source consumers moving by routing static calls through the default LOGIT
 * loaded from configuration.</p>
 */
public final class LibLogIt {
    private static Map<String, Logit> registry = new LinkedHashMap<>(Map.of("default", new Logit()));

    private LibLogIt() {
    }

    /** Load a config file and make its default LOGIT available to static calls. */
    public static synchronized void initFromConfig(String path) throws IOException {
        registry = new LinkedHashMap<>(Logit.loadLogits(path));
    }

    /** Load and return named LOGIT objects from a config file. */
    public static synchronized Map<String, Logit> loadLogits(String path) throws IOException {
        registry = new LinkedHashMap<>(Logit.loadLogits(path));
        return Map.copyOf(registry);
    }

    /** Return a loaded LOGIT by name. */
    public static synchronized Logit getLogit(String name) {
        Logit logit = registry.get(name);
        if (logit == null) {
            throw new IllegalArgumentException("No LOGIT named '" + name + "' has been loaded");
        }
        return logit;
    }

    /** Return a streaming builder bound to the default LOGIT. */
    public static Logit.LogBuilder LOG(String level) {
        return getLogit("default").at(level);
    }

    /** Emit one message through the default LOGIT. */
    public static void log(String level, Object message) {
        getLogit("default").log(level, message);
    }
}
