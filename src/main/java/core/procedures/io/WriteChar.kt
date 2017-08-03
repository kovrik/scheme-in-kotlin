package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.OutputPort

class WriteChar : AFn<Any?, Unit>(name = "write-char", minArgs = 1, maxArgs = 2,
                                  mandatoryArgsTypes = arrayOf<Class<*>>(Char::class.javaObjectType),
                                  restArgsType = OutputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) {
        val outputPort = if (args.size == 1) Repl.currentOutputPort else args[1] as OutputPort
        outputPort.write((args[0] as Char).toInt())
    }
}
