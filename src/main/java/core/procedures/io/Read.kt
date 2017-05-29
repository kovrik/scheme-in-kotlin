package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.FnArgs
import core.reader.Reader
import core.scm.Cons
import core.scm.InputPort
import core.scm.Thunk
import core.scm.specialforms.Begin

class Read : AFn(FnArgs(max = 1, rest = InputPort::class.java)) {

    override val name = "read"

    override operator fun invoke(vararg args: Any?): Any {
        val inputPort: InputPort = if (args.isEmpty()) Repl.currentInputPort else args[0] as InputPort
        val sexps = Cons.list<Any>(Begin.BEGIN)
        sexps.addAll(Reader(inputPort.inputStream).read())
        return Thunk(sexps)
    }
}
