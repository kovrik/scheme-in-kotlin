package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.scm.InputPort

import java.io.IOException

class ReadChar : AFn(name = "read-char", maxArgs = 1, restArgsType = InputPort::class.java) {

    override operator fun invoke(vararg args: Any?): Any? {
        val inputPort: InputPort = if (args.isEmpty()) Repl.currentInputPort else args[0] as InputPort
        try {
            return inputPort.read().toChar()
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
    }
}
