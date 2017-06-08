package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.OutputPort
import core.writer.Writer

import java.io.IOException

class Println : AFn(FnArgs(min = 1, max = 2, mandatory = arrayOf<Class<*>>(Any::class.java), rest = OutputPort::class.java)) {

    override val name = "println"

    override operator fun invoke(vararg args: Any?) {
        val outputPort: OutputPort = if (args.size == 1) Repl.currentOutputPort else args[1] as OutputPort
        try {
            when (args[0]) {
                is CharSequence, is Char -> outputPort.writeln(args[0].toString())
                else -> outputPort.writeln(Writer.write(args[0]))
            }
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
    }
}
