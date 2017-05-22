package core.procedures.io

import core.Repl
import core.exceptions.SCMIOException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.InputPort

import java.io.IOException

class IsCharReady : AFn(FnArgsBuilder().max(1).rest(InputPort::class.java).build()) {

    override val name: String
        get() = "char-ready?"

    override fun apply(args: Array<Any?>): Boolean {
        val inputPort: InputPort
        if (args.isEmpty()) {
            inputPort = Repl.currentInputPort
        } else {
            inputPort = args[0] as InputPort
        }
        val bytesAvailable: Int
        try {
            bytesAvailable = inputPort.available()
        } catch (e: IOException) {
            throw SCMIOException(e)
        }
        return bytesAvailable > 0
    }
}
