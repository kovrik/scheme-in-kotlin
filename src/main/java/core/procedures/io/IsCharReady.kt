package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.InputPort

import java.io.IOException

class IsCharReady : AFn(FnArgs(max = 1, rest = InputPort::class.java)) {

    override val name = "char-ready?"

    override operator fun invoke(vararg args: Any?): Boolean {
        val inputPort: InputPort = if (args.isEmpty()) Repl.currentInputPort else args[0] as InputPort
        try {
            return inputPort.available() > 0
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
    }
}
