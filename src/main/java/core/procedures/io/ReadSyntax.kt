package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.Arity.Range
import core.reader.Reader
import core.scm.InputPort
import core.scm.Syntax

class ReadSyntax : AFn<Any?, Any>(name = "read-syntax", arity = Range(0, 1), restArgsType = InputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> Syntax(Repl.reader.read())
        else -> Syntax(Reader((args[0] as InputPort).inputStream).read())
    }
}