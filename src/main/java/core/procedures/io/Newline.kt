package core.procedures.io

import core.Repl
import core.exceptions.SCMIOException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.OutputPort
import core.scm.Void

import java.io.IOException

class Newline : AFn(FnArgsBuilder().max(1).rest(OutputPort::class.java).build()) {

    companion object {
        private val LS = System.getProperty("line.separator")
    }

    override val name: String
        get() = "newline"

    override operator fun invoke(vararg args: Any?): Any? {
        val outputPort: OutputPort
        if (args.isEmpty()) {
            outputPort = Repl.currentOutputPort
        } else {
            outputPort = args[0] as OutputPort
        }
        try {
            outputPort.write(LS)
        } catch (e: IOException) {
            throw SCMIOException(e)
        }
        return Void
    }
}
