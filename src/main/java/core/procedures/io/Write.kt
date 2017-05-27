package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.OutputPort
import core.scm.Void
import core.writer.Writer

import java.io.IOException

class Write : AFn(FnArgsBuilder().min(1).max(2).mandatory(arrayOf<Class<*>>(Any::class.java))
        .rest(OutputPort::class.java).build()) {

    override val name = "write"

    override operator fun invoke(vararg args: Any?): Void {
        val outputPort: OutputPort
        if (args.size == 1) {
            outputPort = Repl.currentOutputPort
        } else {
            outputPort = args[1] as OutputPort
        }
        try {
            outputPort.write(Writer.write(args[0]))
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
        return Void
    }
}
