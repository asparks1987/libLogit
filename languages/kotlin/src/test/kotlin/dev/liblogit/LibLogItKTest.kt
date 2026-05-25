package dev.liblogit

import java.nio.file.Files
import java.nio.file.Path

fun main() {
    testDirectKotlinLogit()
    testSharedFixtures()
    println("Kotlin libLogit tests passed")
}

private fun testDirectKotlinLogit() {
    val dir = Files.createTempDirectory("liblogit-kotlin-")
    val logPath = dir.resolve("app.log")
    val logIT = KotlinLogit()
    logIT.localPath = logPath.toString()
    logIT.level = "debug"
    logIT.timestamp = false

    (logIT.at("debug") shl "hello" shl " kotlin").commit()

    val lines = Files.readAllLines(logPath)
    check(lines == listOf("DEBUG hello kotlin")) {
        "Unexpected Kotlin log output: $lines"
    }
}

private fun testSharedFixtures() {
    val fixtureDir = Path.of("tests", "conformance", "fixtures")
    Files.list(fixtureDir).use { stream ->
        stream
            .filter { it.toString().endsWith(".json") }
            .sorted()
            .forEach(::runSharedFixture)
    }
}

@Suppress("UNCHECKED_CAST")
private fun runSharedFixture(fixturePath: Path) {
    val fixture = Logit.parseJson(Files.readString(fixturePath)) as Map<String, Any?>
    val name = fixture["name"].toString()
    val root = Files.createTempDirectory("liblogit-kotlin-$name-")
    val config = deepCopy(fixture["config"]) as MutableMap<String, Any?>
    val logitsConfig = config["logits"] as MutableMap<String, Any?>

    for (rawLogitConfig in logitsConfig.values) {
        val logitConfig = rawLogitConfig as MutableMap<String, Any?>
        rewritePath(logitConfig, "path", root)
        rewritePath(logitConfig, "localPath", root)
        rewritePath(logitConfig, "file_location", root)
        rewritePath(logitConfig, "remotePath", root)
        rewritePath(logitConfig, "remote_path", root)
        rewritePath(logitConfig, "network_path", root)
        rewritePath(logitConfig, "network_file_location", root)
    }

    val configPath = root.resolve("logit.json")
    Files.writeString(configPath, toJson(config))
    val loaded = LibLogItK.loadLogits(configPath.toString())

    val messages = fixture["messages"] as List<Map<String, Any?>>
    for (message in messages) {
        val logit = loaded[message["logger"].toString()]
            ?: error("Fixture $name references missing LOGIT ${message["logger"]}")
        val builder = LOG(logit, message["level"].toString())
        val fragments = message["fragments"] as List<Any?>
        for (fragment in fragments) {
            builder shl renderFragment(fragment)
        }
        builder.commit()
    }

    val expectedFiles = fixture["expected_files"] as Map<String, List<Any?>>
    for ((relativePath, expectedLines) in expectedFiles) {
        val actual = Files.readAllLines(root.resolve(relativePath))
        val expected = expectedLines.map { it.toString() }
        check(actual == expected) {
            "Fixture $name produced unexpected contents for $relativePath: $actual"
        }
    }
}

private fun rewritePath(config: MutableMap<String, Any?>, key: String, root: Path) {
    val value = config[key]
    if (value is String && value.isNotBlank()) {
        config[key] = root.resolve(value).toString()
    }
}

private fun deepCopy(value: Any?): Any? =
    when (value) {
        is Map<*, *> -> value.entries.associate { (key, item) -> key.toString() to deepCopy(item) }.toMutableMap()
        is List<*> -> value.map(::deepCopy).toMutableList()
        else -> value
    }

private fun renderFragment(value: Any?): String =
    when (value) {
        null -> "null"
        is String -> value
        is Map<*, *> -> toJson(value)
        is List<*> -> toJson(value)
        else -> value.toString()
    }

private fun toJson(value: Any?): String =
    when (value) {
        null -> "null"
        is String -> "\"" + escape(value) + "\""
        is Number, is Boolean -> value.toString()
        is Map<*, *> -> value.entries.joinToString(prefix = "{", postfix = "}") { (key, item) ->
            toJson(key.toString()) + ":" + toJson(item)
        }
        is Iterable<*> -> value.joinToString(prefix = "[", postfix = "]") { toJson(it) }
        else -> toJson(value.toString())
    }

private fun escape(value: String): String =
    buildString {
        for (character in value) {
            when (character) {
                '\\' -> append("\\\\")
                '"' -> append("\\\"")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> append(character)
            }
        }
    }
