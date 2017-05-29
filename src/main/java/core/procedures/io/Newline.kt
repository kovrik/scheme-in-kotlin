package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.OutputPort
import core.scm.Void

import java.io.IOException

class Newline : AFn(FnArgs(max = 1, rest = OutputPort::class.java)) {

    companion object {
        private val LS = System.getProperty("line.separator")
    }

    override val name = "newline"

    override operator fun invoke(vararg args: Any?): Any? {
        val outputPort: OutputPort = if (args.isEmpty()) Repl.currentOutputPort else args[0] as OutputPort
        try {
            outputPort.write(LS)
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
        return Void
    }
}
