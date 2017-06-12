package core.procedures.strings

import core.exceptions.WrongTypeException
import core.procedures.AFn

import java.util.regex.Pattern

class ReplaceFirst : AFn<Any?, String>(name = "replace-first", isPure = true, minArgs = 3, maxArgs = 3) {

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): String {
        if (arg1 !is CharSequence) {
            throw WrongTypeException(name, "String", arg1)
        }
        if (arg2 is CharSequence && arg3 is CharSequence) {
            return arg1.toString().replaceFirst(arg2.toString(), arg3.toString())
        }
        // TODO arg2=string/arg3=function of match
        return (arg2 as Pattern).matcher((arg1 as CharSequence?)!!).replaceFirst((arg3 as CharSequence).toString())
    }
}
