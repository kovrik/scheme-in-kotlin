package unittests

import core.Repl
import core.scm.InputPort
import core.scm.OutputPort
import org.junit.Test

import java.io.*

import java.lang.Boolean.FALSE
import java.lang.Boolean.TRUE
import org.junit.Assert.assertEquals

class IOTest : AbstractTest() {

    @Test
    @Throws(FileNotFoundException::class)
    fun testCurrentPorts() {
        assertEquals(OutputPort(System.out), eval("(current-output-port)", env))

        val outputStream = ByteArrayOutputStream(0)
        val currentOutputPort = Repl.currentOutputPort
        Repl.currentOutputPort = OutputPort(outputStream)
        assertEquals(OutputPort(outputStream), eval("(current-output-port)", env))
        Repl.currentOutputPort = currentOutputPort

        val inputStream = ByteArrayInputStream("test".toByteArray())
        val currentInputPort = Repl.currentInputPort
        Repl.currentInputPort = InputPort(inputStream)
        assertEquals(InputPort(inputStream), eval("(current-input-port)", env))
        Repl.currentInputPort = currentInputPort
    }

    @Test
    fun testEofObject() {
        assertEquals(TRUE, eval("(eof-object? eof)", env))
        assertEquals(FALSE, eval("(eof-object? 0)", env))
        assertEquals(FALSE, eval("(eof-object? \"test\")", env))
    }

    @Test
    fun testPortPredicates() {
        assertEquals(TRUE, eval("(port? (current-input-port))", env))
        assertEquals(TRUE, eval("(port? (current-output-port))", env))
        assertEquals(TRUE, eval("(input-port? (current-input-port))", env))
        assertEquals(TRUE, eval("(output-port? (current-output-port))", env))

        assertEquals(FALSE, eval("(port? 1)", env))
        assertEquals(FALSE, eval("(output-port? (current-input-port))", env))
        assertEquals(FALSE, eval("(input-port? (current-output-port))", env))
    }

    // TODO close-*-port, write-char, peek-char, read-char, read, write
}
