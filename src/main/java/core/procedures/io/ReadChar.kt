package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.InputPort

class ReadChar : AFn<Any?, Char>(name = "read-char", maxArgs = 1, restArgsType = InputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>): Char {
        val inputPort = if (args.isEmpty()) Repl.currentInputPort else args[0] as InputPort
        return inputPort.read().toChar()
    }
}
