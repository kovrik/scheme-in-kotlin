package core.procedures.io

import core.Repl
import core.exceptions.SCMIOException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.InputPort

import java.io.IOException

class PeekChar : AFn(FnArgsBuilder().max(1).rest(InputPort::class.java).build()) {

    override val name: String
        get() = "peek-char"

    override operator fun invoke(vararg args: Any?): Any? {
        val inputPort: InputPort
        if (args.isEmpty()) {
            inputPort = Repl.currentInputPort
        } else {
            inputPort = args[0] as InputPort
        }
        try {
            return inputPort.peek().toChar()
        } catch (e: IOException) {
            throw SCMIOException(e)
        }
    }
}
