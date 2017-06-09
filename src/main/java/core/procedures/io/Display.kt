package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.scm.OutputPort
import core.writer.Writer
import java.io.IOException

open class Display : AFn(name = "display", minArgs = 1, maxArgs = 2, mandatoryArgsTypes = arrayOf<Class<*>>(Any::class.java), restArgsType = OutputPort::class.java) {

    override operator fun invoke(vararg args: Any?) {
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
    }
}
