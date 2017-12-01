package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.OutputPort
import core.writer.Writer

class Write : AFn<Any?, Unit>(name = "write", minArgs = 1, maxArgs = 2, mandatoryArgsTypes = arrayOf(Any::class.java),
                              restArgsType = OutputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> Repl.currentOutputPort
        else -> args[1] as OutputPort
    }.write(Writer.write(args[0]))
}
