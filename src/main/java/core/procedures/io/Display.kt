package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.OutputPort
import core.Writer

open class Display : AFn<Any?, Unit>(name = "display", minArgs = 1, maxArgs = 2,
                                     mandatoryArgsTypes = arrayOf(Any::class.java),
                                     restArgsType = OutputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> Repl.currentOutputPort
        else -> args[1]!! as OutputPort
    }.let {
        val arg = args[0]!!
        when (arg) {
            is CharSequence, is Char -> it.write(arg.toString())
            else -> it.write(Writer.write(arg))
        }
    }
}
