package unittests

import core.Repl
import core.scm.InputPort
import core.scm.OutputPort
import org.junit.Test

import java.io.*

import org.junit.Assert.assertEquals

class IOTest : AbstractTest() {

    @Test
    @Throws(FileNotFoundException::class)
    fun testCurrentPorts() {
        assertEquals(OutputPort(System.out), eval("(current-output-port)"))

        val outputStream = ByteArrayOutputStream(0)
        val currentOutputPort = Repl.currentOutputPort
        Repl.currentOutputPort = OutputPort(outputStream)
        assertEquals(OutputPort(outputStream), eval("(current-output-port)"))
        Repl.currentOutputPort = currentOutputPort

        val inputStream = ByteArrayInputStream("test".toByteArray())
        val currentInputPort = Repl.currentInputPort
        Repl.currentInputPort = InputPort(inputStream)
        assertEquals(InputPort(inputStream), eval("(current-input-port)"))
        Repl.currentInputPort = currentInputPort
    }

    @Test
    fun testEofObject() {
        assertEquals(true, eval("(eof-object? eof)"))
        assertEquals(false, eval("(eof-object? 0)"))
        assertEquals(false, eval("(eof-object? \"test\")"))
    }

    @Test
    fun testPortPredicates() {
        assertEquals(true, eval("(port? (current-input-port))"))
        assertEquals(true, eval("(port? (current-output-port))"))
        assertEquals(true, eval("(input-port? (current-input-port))"))
        assertEquals(true, eval("(output-port? (current-output-port))"))

        assertEquals(false, eval("(port? 1)"))
        assertEquals(false, eval("(output-port? (current-input-port))"))
        assertEquals(false, eval("(input-port? (current-output-port))"))
    }

    // TODO close-*-port, write-char, peek-char, read-char, read, write
}
