package core.procedures.strings

import core.procedures.AFn

import java.util.regex.Matcher
import java.util.regex.Pattern

class ReMatcher : AFn(name = "re-matcher", isPure = true, minArgs = 2, maxArgs = 2,
                      mandatoryArgsTypes = arrayOf(Pattern::class.java, CharSequence::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Matcher = (arg1 as Pattern).matcher((arg2 as CharSequence?)!!)
}
