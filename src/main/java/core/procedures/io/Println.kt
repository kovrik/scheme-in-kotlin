package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.OutputPort
import core.Writer
import core.procedures.Arity.Range

class Println : AFn<Any?, Unit>(name = "println", arity = Range(1, 2),
                                mandatoryArgsTypes = arrayOf(Any::class.java), restArgsType = OutputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> Repl.currentOutputPort
        else -> args[1] as OutputPort
    }.let {
        when (args[0]) {
            is CharSequence, is Char -> it.writeln(args[0].toString())
            else -> it.writeln(Writer.write(args[0]))
        }
    }
}
