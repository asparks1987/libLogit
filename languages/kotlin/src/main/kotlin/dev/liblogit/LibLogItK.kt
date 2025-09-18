package dev.liblogit

object LibLogItK {
    @JvmStatic
    @Throws(java.io.IOException::class)
    fun initFromConfig(path: String) {
        LibLogIt.initFromConfig(path)
    }

    fun LOG(level: String): KotlinLogBuilder = KotlinLogBuilder(level)

    class KotlinLogBuilder internal constructor(private val level: String) {
        private val buffer = StringBuilder()
        private var committed = false

        operator fun shl(value: Any?): KotlinLogBuilder {
            if (!committed) {
                buffer.append(value?.toString() ?: "null")
            }
            return this
        }

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
