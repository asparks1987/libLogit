import dev.liblogit.KotlinLogit
import dev.liblogit.LibLogItK

fun main() {
    val LogIT = KotlinLogit()
    LogIT.localPath = "logs/kotlin-app.log"
    LogIT.level = "debug"

    (LogIT.at("info") shl "Kotlin app started").commit()

    val configured = LibLogItK.loadLogits("examples/config/v2-basic.json")
    (configured.getValue("AppLog").at("info").append("configured Kotlin log")).commit()
}
