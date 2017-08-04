package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.InputPort

class IsCharReady : AFn<Any?, Boolean>(name = "char-ready?", maxArgs = 1, restArgsType = InputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> Repl.currentInputPort
        else -> args[0]!! as InputPort
    }.available() > 0
}
