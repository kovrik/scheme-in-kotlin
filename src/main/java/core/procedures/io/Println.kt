package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.OutputPort
import core.scm.Void
import core.writer.Writer

import java.io.IOException

class Println : AFn(FnArgs(min = 1, max = 2, mandatory = arrayOf<Class<*>>(Any::class.java), rest = OutputPort::class.java)) {

    override val name = "println"

    override operator fun invoke(vararg args: Any?): Any? {
        val outputPort: OutputPort = if (args.size == 1) Repl.currentOutputPort else args[1] as OutputPort
        val arg = args[0]
        try {
            when (arg) {
                is CharSequence, is Char -> outputPort.writeln(arg.toString())
                else -> outputPort.writeln(Writer.write(arg))
            }
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
        return Void
    }
}
