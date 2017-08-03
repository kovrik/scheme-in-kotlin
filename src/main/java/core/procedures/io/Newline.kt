package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.OutputPort


class Newline : AFn<Any?, Unit>(name = "newline", maxArgs = 1, restArgsType = OutputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) {
        val outputPort = if (args.isEmpty()) Repl.currentOutputPort else args[0] as OutputPort
        outputPort.writeln("")
    }
}
