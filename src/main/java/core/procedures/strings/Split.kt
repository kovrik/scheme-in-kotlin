package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder

import core.scm.Vector
import java.util.regex.Pattern

class Split : AFn(FnArgsBuilder().min(2).max(3).mandatory(arrayOf(CharSequence::class.java, Pattern::class.java)).rest(Long::class.java).build()) {

    override val name: String
        get() = "split"

    override operator fun invoke(vararg args: Any?): Vector {
        when {
            args.size == 2 -> return Vector(*(args[1] as Pattern).split(args[0] as CharSequence))
            else           -> return Vector(*(args[1] as Pattern).split(args[0] as CharSequence, (args[2] as Long).toInt()))
        }
    }
}
