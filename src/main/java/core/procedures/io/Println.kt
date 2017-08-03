package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.OutputPort
import core.writer.Writer

class Println : AFn<Any?, Unit>(name = "println", minArgs = 1, maxArgs = 2,
                    mandatoryArgsTypes = arrayOf<Class<*>>(Any::class.java), restArgsType = OutputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) {
        val outputPort = if (args.size == 1) Repl.currentOutputPort else args[1] as OutputPort
        when (args[0]) {
            is CharSequence, is Char -> outputPort.writeln(args[0].toString())
            else -> outputPort.writeln(Writer.write(args[0]))
        }
    }
}
