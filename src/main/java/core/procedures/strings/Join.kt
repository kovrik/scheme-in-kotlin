package core.procedures.strings

import core.Writer
import core.procedures.AFn
import core.procedures.Arity.Range
import core.utils.Utils

class Join : AFn<Any?, String>(name = "join", isPure = true, arity = Range(1, 2),
                               mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> args[0].toString()
        else -> Utils.toSequence(args[1]).joinToString(separator = args[0].toString()) { str(it) }
    }

    private fun str(obj: Any?) = when (obj) {
        null -> ""
        is Char -> obj.toString()
        is CharSequence -> obj
        else -> Writer.write(obj)
    }
}
