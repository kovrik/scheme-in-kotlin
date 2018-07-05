package core.procedures.strings

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.Range

import java.util.regex.Matcher
import java.util.regex.Pattern

class ReFind : AFn<Any?, Any?>(name = "re-find", isPure = true, arity = Range(1, 2)) {

    private val reGroups = ReGroups()

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1 -> {
            val matcher = args[0] as? Matcher ?: throw WrongTypeException(name, Matcher::class.java, args[0])
            if (matcher.find()) reGroups(args[0]) else null
        }
        else -> {
            val pattern = args[0] as? Pattern ?: throw WrongTypeException(name, Pattern::class.java, args[0])
            val chars = args[1] as? CharSequence ?: throw WrongTypeException(name, CharSequence::class.java, args[1])
            val matcher = pattern.matcher(chars)
            if (matcher.find()) matcher.group() else null
        }
    }
}
