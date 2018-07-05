package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.OutputPort

class WriteChar : AFn<Any?, Unit>(name = "write-char", arity = Range(1, 2),
                                  mandatoryArgsTypes = arrayOf(Char::class.javaObjectType),
                                  restArgsType = OutputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> Repl.currentOutputPort
        else -> args[1] as OutputPort
    }.write((args[0] as Char).toInt())
}
