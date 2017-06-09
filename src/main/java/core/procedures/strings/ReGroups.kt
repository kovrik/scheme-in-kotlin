package core.procedures.strings

import core.procedures.AFn
import core.scm.MutableVector

import java.util.regex.Matcher

class ReGroups : AFn(name = "re-groups", isPure = true, minArgs = 1, maxArgs = 1,
                     mandatoryArgsTypes = arrayOf<Class<*>>(Matcher::class.java)) {

    override operator fun invoke(arg: Any?): Any? {
        val m = arg!! as Matcher
        val gc = m.groupCount()
        if (gc == 0) {
            return m.group()
        }
        val result = MutableVector(gc + 1, null)
        for (c in 0..gc) {
            result.getArray()[c] = m.group(c)
        }
        return result
    }
}
