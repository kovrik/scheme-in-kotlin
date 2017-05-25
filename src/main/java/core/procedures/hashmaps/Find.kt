package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.IAssoc
import core.scm.MapEntry
import core.utils.Utils

class Find : AFn(FnArgsBuilder().max(2).min(2).mandatory(arrayOf(IAssoc::class.java, Any::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "find"

    override operator fun invoke(arg1: Any?, arg2: Any?): MapEntry? {
        val assoc = Utils.toAssoc(arg1)
        if (assoc.containsKey(arg2!!)) {
            return assoc.getEntry(arg2)
        }
        return null
    }
}
