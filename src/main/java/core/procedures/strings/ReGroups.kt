package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutableVector

import java.util.regex.Matcher

class ReGroups : AFn<Matcher?, Any?>(name = "re-groups", isPure = true, arity = Exactly(1),
                                     mandatoryArgsTypes = arrayOf(Matcher::class.java)) {

    override operator fun invoke(arg: Matcher?): Any? = when (val count = arg!!.groupCount()) {
        0 -> arg.group()
        else -> MutableVector(count + 1, null).apply {
            for (c in 0..count) {
                array[c] = arg.group(c)
            }
        }
    }
}
