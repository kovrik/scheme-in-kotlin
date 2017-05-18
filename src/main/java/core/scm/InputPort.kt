package core.scm

import java.io.IOException
import java.io.InputStream

class InputPort(val inputStream: InputStream) : IPort {

    private val lock = Any()

    private var next: Int? = null

    @Throws(IOException::class)
    override fun close() {
        inputStream.close()
    }

    @Throws(IOException::class)
    fun read(): Int {
        synchronized(lock) {
            if (next != null) {
                val result = next!!
                next = null
                return result
            } else {
                return inputStream.read()
            }
        }
    }

    @Throws(IOException::class)
    fun peek(): Int {
        synchronized(lock) {
            if (next == null) {
                next = inputStream.read()
            }
            return next!!
        }
    }

    @Throws(IOException::class)
    fun available(): Int {
        return inputStream.available()
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || javaClass != other.javaClass) return false
        val that = other as InputPort?
        return inputStream == that!!.inputStream
    }

    override fun hashCode(): Int {
        return inputStream.hashCode()
    }

    override fun toString(): String {
        if (inputStream == System.`in`) {
            return "#<input-port:stdin>"
        }
        return "#<input-port:$inputStream>"
    }
}
