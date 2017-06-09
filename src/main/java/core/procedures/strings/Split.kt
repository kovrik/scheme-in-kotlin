package core.procedures.strings

import core.procedures.AFn

import core.scm.Vector
import java.util.regex.Pattern

class Split : AFn(name = "split", isPure = true, minArgs = 2, maxArgs = 3,
                  mandatoryArgsTypes = arrayOf(CharSequence::class.java, Pattern::class.java),
                  restArgsType = Long::class.java) {

    override operator fun invoke(vararg args: Any?): Vector {
        return when (args.size) {
            2    -> Vector(*(args[1] as Pattern).split(args[0] as CharSequence))
            else -> Vector(*(args[1] as Pattern).split(args[0] as CharSequence, (args[2] as Long).toInt()))
        }
    }
}
