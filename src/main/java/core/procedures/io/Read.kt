package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.reader.Reader
import core.scm.Cons
import core.scm.InputPort
import core.scm.Thunk
import core.scm.specialforms.Begin

class Read : AFn<Any?, Any>(name = "read", maxArgs = 1, restArgsType = InputPort::class.java) {

    override operator fun invoke(args: Array<Any?>): Any {
        val inputPort: InputPort = if (args.isEmpty()) Repl.currentInputPort else args[0] as InputPort
        val sexps = Cons.list<Any>(Begin.BEGIN)
        sexps.addAll(Reader(inputPort.inputStream).read())
        return Thunk(sexps)
    }
}
