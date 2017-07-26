package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.reader.Reader
import core.scm.Cons
import core.scm.InputPort
import core.scm.Thunk
import core.scm.specialforms.Begin

class Read : AFn<Any?, Any>(name = "read", maxArgs = 1, restArgsType = InputPort::class.java) {

    // FIXME doesn't work if we want to read multiple forms at once. Implement properly!
    override operator fun invoke(args: Array<out Any?>): Any {
        val inputPort = if (args.isEmpty()) Repl.currentInputPort else args[0] as InputPort
        val sexps = Cons.list<Any>(Begin)
        while (true) {
            val token = Reader(inputPort.inputStream).nextToken() ?: break
            sexps.add(token)
        }
        return Thunk(sexps)
    }
}
