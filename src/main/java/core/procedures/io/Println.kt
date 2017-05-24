package core.procedures.io

import core.Repl
import core.exceptions.SCMIOException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.OutputPort
import core.scm.Void
import core.writer.Writer

import java.io.IOException

class Println : AFn(FnArgsBuilder().min(1).max(2).mandatory(arrayOf<Class<*>>(Any::class.java))
        .rest(OutputPort::class.java).build()) {

    override val name: String
        get() = "println"

    override fun apply(vararg args: Any?): Any? {
        val outputPort: OutputPort
        if (args.size == 1) {
            outputPort = Repl.currentOutputPort
        } else {
            outputPort = args[1] as OutputPort
        }
        val arg = args[0]
        try {
            if (arg is CharSequence || arg is Char) {
                outputPort.writeln(arg.toString())
            } else {
                outputPort.writeln(Writer.write(arg))
            }
        } catch (e: IOException) {
            throw SCMIOException(e)
        }
        return Void.VOID
    }
}
