package core.exceptions

import core.Writer

class ExInfoException : RuntimeException {

    val info: Map<*, *>

    constructor(message: String, info: Map<*, *>) : super(message) {
        this.info = info
    }

    constructor(message: String, info: Map<*, *>, cause: Throwable) : super(message, cause) {
        this.info = info
    }

    @Synchronized override fun fillInStackTrace() = null

    override fun toString() = "#<ex-info:{message:$message, cause: ${Writer.write(cause)}, data: ${Writer.write(info)}}>"
}
