/** Kotlin façade over the shared libLogit JSON configuration. */
package dev.liblogit

object LibLogItK {
    @JvmStatic
    @Throws(java.io.IOException::class)
    fun initFromConfig(path: String) {
        LibLogIt.initFromConfig(path)
    }

    /** Return a streaming builder compatible with the JVM API. */
    fun LOG(level: String): KotlinLogBuilder = KotlinLogBuilder(level)

    class KotlinLogBuilder internal constructor(private val level: String) {
        private val buffer = StringBuilder()
        private var committed = false

        /** Append a fragment to the buffered log message. */
        operator fun shl(value: Any?): KotlinLogBuilder {
            if (!committed) {
                buffer.append(value?.toString() ?: "null")
            }
            return this
        }

        /** Flush buffered fragments to the underlying Java implementation. */
        fun commit(): KotlinLogBuilder {
            if (!committed) {
                committed = true
                LibLogIt.log(level, buffer.toString())
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

fun LOG(level: String): LibLogItK.KotlinLogBuilder = LibLogItK.LOG(level)
