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
        assertEquals(true, eval("(eof-object? eof)", env))
        assertEquals(false, eval("(eof-object? 0)", env))
        assertEquals(false, eval("(eof-object? \"test\")", env))
    }

    @Test
    fun testPortPredicates() {
        assertEquals(true, eval("(port? (current-input-port))", env))
        assertEquals(true, eval("(port? (current-output-port))", env))
        assertEquals(true, eval("(input-port? (current-input-port))", env))
        assertEquals(true, eval("(output-port? (current-output-port))", env))

        assertEquals(false, eval("(port? 1)", env))
        assertEquals(false, eval("(output-port? (current-input-port))", env))
        assertEquals(false, eval("(input-port? (current-output-port))", env))
    }

    // TODO close-*-port, write-char, peek-char, read-char, read, write
}
