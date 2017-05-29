package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs

import core.scm.Vector
import java.util.regex.Pattern

class Split : AFn(FnArgs(min = 2, max = 3, mandatory = arrayOf(CharSequence::class.java, Pattern::class.java), rest = Long::class.java)) {

    override val name = "split"

    override operator fun invoke(vararg args: Any?): Vector {
        when (args.size) {
            2    -> return Vector(*(args[1] as Pattern).split(args[0] as CharSequence))
            else -> return Vector(*(args[1] as Pattern).split(args[0] as CharSequence, (args[2] as Long).toInt()))
        }
    }
}
