package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.OutputPort

import java.io.IOException

class Newline : AFn(FnArgs(max = 1, rest = OutputPort::class.java)) {

    companion object {
        private val LS = System.getProperty("line.separator")
    }

    override val name = "newline"

    override operator fun invoke(vararg args: Any?) = try {
        val outputPort: OutputPort = if (args.isEmpty()) Repl.currentOutputPort else args[0] as OutputPort
        outputPort.write(LS)
    } catch (e: IOException) {
        throw ThrowableWrapper(e)
    }
}
