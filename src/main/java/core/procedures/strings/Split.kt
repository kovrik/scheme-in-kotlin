package core.procedures.strings

import core.procedures.AFn

import core.scm.Vector
import java.util.regex.Pattern

class Split : AFn<Any?, Vector>(name = "split", isPure = true, minArgs = 2, maxArgs = 3,
                                mandatoryArgsTypes = arrayOf(CharSequence::class.java, Pattern::class.java),
                                restArgsType = Long::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        2    -> Vector((args[1] as Pattern).split(args[0] as CharSequence) as Array<Any?>)
        else -> Vector((args[1] as Pattern).split(args[0] as CharSequence, (args[2] as Long).toInt()) as Array<Any?>)
    }
}
