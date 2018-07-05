package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly

import java.util.regex.Matcher
import java.util.regex.Pattern

class ReMatcher : AFn<Any?, Matcher>(name = "re-matcher", isPure = true, arity = Exactly(2),
                                     mandatoryArgsTypes = arrayOf(Pattern::class.java, CharSequence::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Matcher = (arg1 as Pattern).matcher((arg2 as CharSequence?)!!)
}
