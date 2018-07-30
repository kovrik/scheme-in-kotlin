package core.exceptions

import core.Writer

class ExInfoException(message: String, val info: Map<*, *>, cause: Throwable?) : RuntimeException(message, cause) {

    @Synchronized override fun fillInStackTrace() = null

    override fun toString() = "#<ex-info:{message:$message, cause: ${Writer.write(cause)}, data: ${Writer.write(info)}}>"
}
