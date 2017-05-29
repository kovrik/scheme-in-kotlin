package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.InputPort

import java.io.IOException

class PeekChar : AFn(FnArgs(max = 1, rest = InputPort::class.java)) {

    override val name = "peek-char"

    override operator fun invoke(vararg args: Any?): Any? {
        val inputPort: InputPort = if (args.isEmpty()) Repl.currentInputPort else args[0] as InputPort
        try {
            return inputPort.peek().toChar()
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
    }
}
