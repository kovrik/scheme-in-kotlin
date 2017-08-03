package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.InputPort

class PeekChar : AFn<Any?, Char>(name = "peek-char", maxArgs = 1, restArgsType = InputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>): Char {
        val inputPort = if (args.isEmpty()) Repl.currentInputPort else args[0] as InputPort
        return inputPort.peek().toChar()
    }
}
