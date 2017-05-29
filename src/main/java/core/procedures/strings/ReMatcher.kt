package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs

import java.util.regex.Matcher
import java.util.regex.Pattern

class ReMatcher : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(Pattern::class.java, CharSequence::class.java))) {

    override val name = "re-matcher"

    override operator fun invoke(arg1: Any?, arg2: Any?): Matcher {
        return (arg1 as Pattern).matcher((arg2 as CharSequence?)!!)
    }
}
