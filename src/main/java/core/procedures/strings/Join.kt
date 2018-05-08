package core.procedures.strings

import core.procedures.AFn
import core.utils.Utils

class Join : AFn<Any?, String>(name = "join", isPure = true, minArgs = 1, maxArgs = 2,
                               mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> args[0].toString()
        else -> Utils.toSequence(args[1]).joinToString(args[0].toString())
    }
}
