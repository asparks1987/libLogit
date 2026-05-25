import dev.liblogit.Logit;

import java.util.Map;

public final class Basic {
    public static void main(String[] args) throws Exception {
        Logit logIT = new Logit();
        logIT.setLocalPath("logs/java-app.log");
        logIT.level = Logit.Level.DEBUG;

        logIT.at(Logit.Level.INFO).append("Java app started").commit();

        Map<String, Logit> configured = Logit.loadLogits("examples/config/v2-basic.json");
        configured.get("AppLog").at(Logit.Level.INFO).append("configured Java log").commit();
    }
}
