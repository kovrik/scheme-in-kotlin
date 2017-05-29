package core.procedures.strings

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs

import java.util.regex.Pattern

class Replace : AFn(FnArgs(min = 3, max = 3)) {

    override val name = "replace"

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): String? {
        if (arg1 !is CharSequence) {
            throw WrongTypeException(name, "String", arg1)
        }
        if (arg2 is Char && arg3 is Char) {
            return arg1.toString().replace((arg2 as Char?)!!, (arg3 as Char?)!!)
        }
        if (arg2 is CharSequence && arg3 is CharSequence) {
            return arg1.toString().replace(arg2.toString(), arg3.toString())
        }
        // TODO arg2=string/arg3=function of match
        return (arg2 as Pattern).matcher((arg1 as CharSequence?)!!).replaceAll((arg3 as CharSequence).toString())
    }
}
