package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.OutputPort


class Newline : AFn<Any?, Unit>(name = "newline", arity = Range(0, 1), restArgsType = OutputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> Repl.currentOutputPort
        else -> args[0] as OutputPort
    }.writeln("")
}
