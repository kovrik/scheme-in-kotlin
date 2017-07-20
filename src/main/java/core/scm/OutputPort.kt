package core.scm

import java.io.IOException
import java.io.OutputStream

class OutputPort(val outputStream: OutputStream?) : IPort {

    companion object {
        private val LS = System.getProperty("line.separator")
    }

    @Throws(IOException::class)
    override fun close() = outputStream!!.close()

    @Throws(IOException::class)
    fun write(b: Int) = outputStream!!.write(b)

    @Throws(IOException::class)
    fun write(str: String) = outputStream!!.write(str.toByteArray())

    @Throws(IOException::class)
    fun writeln(str: String) = outputStream!!.write((str + LS).toByteArray())

    override fun equals(other: Any?) = when {
        this === other -> true
        other == null || javaClass != other.javaClass -> false
        else -> outputStream== (other as OutputPort).outputStream
    }

    override fun hashCode() = outputStream?.hashCode() ?: 0

    override fun toString(): String = when (outputStream) {
        System.out -> "#<output-port:stdout>"
        else -> "#<output-port:$outputStream>"
    }
}
