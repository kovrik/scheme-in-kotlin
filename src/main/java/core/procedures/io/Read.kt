package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.reader.Reader
import core.scm.Cons
import core.scm.InputPort
import core.scm.specialforms.Begin
import core.scm.Thunk

class Read : AFn(FnArgsBuilder().max(1).rest(InputPort::class.java).build()) {

    override val name = "read"

    override operator fun invoke(vararg args: Any?): Any {
        val inputPort: InputPort
        if (args.isEmpty()) {
            inputPort = Repl.currentInputPort
        } else {
            inputPort = args[0] as InputPort
        }
        val sexps = Cons.list<Any>(Begin.BEGIN)
        sexps.addAll(Reader(inputPort.inputStream).read())
        return Thunk(sexps)
    }
}
