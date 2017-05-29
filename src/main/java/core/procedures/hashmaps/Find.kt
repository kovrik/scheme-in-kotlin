package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.IAssoc
import core.scm.MapEntry
import core.utils.Utils

class Find : AFn(FnArgs(max = 2, min = 2, mandatory = arrayOf(IAssoc::class.java, Any::class.java))) {

    override val isPure = true
    override val name = "find"

    override operator fun invoke(arg1: Any?, arg2: Any?): MapEntry? {
        val assoc = Utils.toAssoc(arg1)
        if (assoc.containsKey(arg2!!)) {
            return assoc.getEntry(arg2)
        }
        return null
    }
}
