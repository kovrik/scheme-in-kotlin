package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.OutputPort
import core.scm.Void

import java.io.IOException

class WriteChar : AFn(FnArgsBuilder().min(1).max(2).mandatory(arrayOf<Class<*>>(Char::class.javaObjectType))
        .rest(OutputPort::class.java).build()) {

    override val name = "write-char"

    override operator fun invoke(vararg args: Any?): Void {
        val ch = args[0] as Char
        val outputPort: OutputPort
        if (args.size == 1) {
            outputPort = Repl.currentOutputPort
        } else {
            outputPort = args[1] as OutputPort
        }
        try {
            outputPort.write(ch.toInt())
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
        return Void
    }
}
