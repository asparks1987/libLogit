package dev.liblogit

import java.io.IOException

/** Kotlin facade over the shared LOGIT object model. */
object LibLogItK {
    @JvmStatic
    @Throws(IOException::class)
    fun loadLogits(path: String): Map<String, Logit> = Logit.loadLogits(path)

    /** Return a streaming builder bound to a Java LOGIT object. */
    fun LOG(logit: Logit, level: String): KotlinLogBuilder = KotlinLogBuilder(logit, level)

    /** Return a streaming builder bound to a Kotlin wrapper LOGIT object. */
    fun LOG(logit: KotlinLogit, level: String): KotlinLogBuilder = KotlinLogBuilder(logit.unwrap(), level)

    class KotlinLogBuilder internal constructor(
        private val logit: Logit,
        level: String,
    ) {
        private val parsedLevel = Logit.parseLevel(level)
        private val buffer = StringBuilder()
        private var committed = false

        /** Append a fragment to the buffered log message. */
        infix fun shl(value: Any?): KotlinLogBuilder {
            if (!committed) {
                buffer.append(value?.toString() ?: "null")
            }
            return this
        }

        /** Flush buffered fragments to the underlying Java implementation. */
        fun commit(): KotlinLogBuilder {
            if (!committed && buffer.isNotEmpty()) {
                committed = true
                logit.log(parsedLevel, buffer.toString())
            }
            return this
        }

        @Throws(Throwable::class)
        protected fun finalize() {
            if (!committed) {
                commit()
            }
        }
    }
}

/** Kotlin-native wrapper with mutable startup properties. */
class KotlinLogit(private val delegate: Logit = Logit()) {
    var name: String
        get() = delegate.name
        set(value) {
            delegate.name = value
        }

    var localPath: String?
        get() = delegate.localPath
        set(value) {
            delegate.localPath = value
        }

    var remotePath: String?
        get() = delegate.remotePath
        set(value) {
            delegate.remotePath = value
        }

    var level: String
        get() = delegate.level.name.lowercase()
        set(value) {
            delegate.level = Logit.parseLevel(value)
        }

    var timestamp: Boolean
        get() = delegate.timestamp
        set(value) {
            delegate.timestamp = value
        }

    var tagLevel: Boolean
        get() = delegate.tagLevel
        set(value) {
            delegate.tagLevel = value
        }

    var format: String
        get() = delegate.format
        set(value) {
            delegate.format = value
        }

    fun at(level: String): LibLogItK.KotlinLogBuilder = LibLogItK.LOG(this, level)

    fun log(level: String, message: Any?) {
        delegate.log(level, message)
    }

    fun unwrap(): Logit = delegate
}

fun LOG(logit: Logit, level: String): LibLogItK.KotlinLogBuilder = LibLogItK.LOG(logit, level)

fun LOG(logit: KotlinLogit, level: String): LibLogItK.KotlinLogBuilder = LibLogItK.LOG(logit, level)
