package core.procedures.strings

import core.procedures.AFn
import core.scm.MutableVector

import java.util.regex.Matcher

class ReGroups : AFn<Any?, Any?>(name = "re-groups", isPure = true, minArgs = 1, maxArgs = 1,
                                 mandatoryArgsTypes = arrayOf(Matcher::class.java)) {

    override operator fun invoke(arg: Any?): Any? {
        val matcher = arg!! as Matcher
        val groupCount = matcher.groupCount()
        return when (groupCount) {
            0    -> matcher.group()
            else -> MutableVector(groupCount + 1, null).apply {
                for (c in 0..groupCount) {
                    array[c] = matcher.group(c)
                }
            }
        }
    }
}
