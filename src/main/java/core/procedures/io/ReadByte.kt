package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.InputPort

class ReadByte : AFn<Any?, Byte>(name = "read-byte", maxArgs = 1, restArgsType = InputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> Repl.currentInputPort
        else -> args[0] as InputPort
    }.read().toByte()
}