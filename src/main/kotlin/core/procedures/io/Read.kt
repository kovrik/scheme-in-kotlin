package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.reader.Reader
import core.scm.InputPort
import core.scm.Thunk

class Read : AFn<Any?, Any>(name = "read", maxArgs = 1, restArgsType = InputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> Thunk(Repl.reader.read())
        else -> Thunk(Reader((args[0] as InputPort).inputStream).read())
    }
}
