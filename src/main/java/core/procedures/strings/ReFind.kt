package core.procedures.strings

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs

import java.util.regex.Matcher
import java.util.regex.Pattern

class ReFind : AFn(FnArgs(min = 1, max = 2)) {

    private val reGroups = ReGroups()

    override val name = "re-find"

    override operator fun invoke(vararg args: Any?): Any? {
        if (args.size == 1) {
            if (args[0] !is Matcher) {
                throw WrongTypeException(name, Matcher::class.java, args[0])
            }
            val m = args[0] as Matcher
            return if (m.find()) reGroups(args[0]) else null
        }
        if (args[0] !is Pattern) {
            throw WrongTypeException(name, Pattern::class.java, args[0])
        }
        if (args[1] !is CharSequence) {
            throw WrongTypeException(name, CharSequence::class.java, args[1])
        }
        val matcher = (args[0] as Pattern).matcher(args[1] as CharSequence)
        return if (matcher.find()) matcher.group() else null
    }
}
