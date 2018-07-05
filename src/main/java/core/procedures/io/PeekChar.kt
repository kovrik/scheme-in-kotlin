package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.InputPort

class PeekChar : AFn<Any?, Char>(name = "peek-char", arity = Range(0, 1), restArgsType = InputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> Repl.currentInputPort
        else -> args[0] as InputPort
    }.peek().toChar()
}
