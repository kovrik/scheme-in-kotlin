package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.MutableVector

import java.util.regex.Matcher

class ReGroups : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Matcher::class.java)).build()) {

    override val name = "re-groups"

    override operator fun invoke(arg: Any?): Any? {
        val m = arg as Matcher?
        val gc = m!!.groupCount()
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
