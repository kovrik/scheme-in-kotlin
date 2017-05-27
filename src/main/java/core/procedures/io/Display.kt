package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.OutputPort
import core.scm.Void
import core.writer.Writer

import java.io.IOException

open class Display : AFn(FnArgsBuilder().min(1).max(2).mandatory(arrayOf<Class<*>>(Any::class.java))
        .rest(OutputPort::class.java).build()) {

    override val name = "display"

    override operator fun invoke(vararg args: Any?): Any? {
        val outputPort: OutputPort
        when {
            args.size == 1 -> outputPort = Repl.currentOutputPort
            else -> outputPort = args[1] as OutputPort
        }
        val arg = args[0]
        try {
            when (arg) {
                is CharSequence, is Char -> outputPort.write(arg.toString())
                else -> outputPort.write(Writer.write(arg))
            }
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
        return Void
    }
}
