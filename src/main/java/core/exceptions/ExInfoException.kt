package core.exceptions

import core.writer.Writer

class ExInfoException : RuntimeException {

    val info: Map<*, *>

    constructor(message: String, info: Map<*, *>) : super(message) {
        this.info = info
    }

    constructor(message: String, info: Map<*, *>, cause: Throwable) : super(message, cause) {
        this.info = info
    }

    @Synchronized override fun fillInStackTrace(): Throwable? {
        return null
    }

    override fun toString(): String {
        return "#<ex-info:{message:" + message +
                ", cause: " + Writer.write(cause) +
                ", data: " + Writer.write(info) + "}>"
    }
}
