package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.scm.OutputPort

import java.io.IOException

class Newline : AFn<Any?, Unit>(name = "newline", maxArgs = 1, restArgsType = OutputPort::class.java) {

    companion object {
        private val LS = System.getProperty("line.separator")
    }

    override operator fun invoke(vararg args: Any?) = try {
        val outputPort: OutputPort = if (args.isEmpty()) Repl.currentOutputPort else args[0] as OutputPort
        outputPort.write(LS)
    } catch (e: IOException) {
        throw ThrowableWrapper(e)
    }
}
