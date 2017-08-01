package core.procedures.strings

import core.exceptions.WrongTypeException
import core.procedures.AFn

import java.util.regex.Pattern

class ReplaceFirst : AFn<Any?, String>(name = "replace-first", isPure = true, minArgs = 3, maxArgs = 3) {

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): String {
        val chars = arg1 as? CharSequence ?: throw WrongTypeException(name, "String", arg1)
        if (arg2 is CharSequence && arg3 is CharSequence) {
            return chars.toString().replaceFirst(arg2.toString(), arg3.toString())
        }
        // TODO arg2=string/arg3=function of match
        return (arg2 as Pattern).matcher(chars).replaceFirst((arg3 as CharSequence).toString())
    }
}
