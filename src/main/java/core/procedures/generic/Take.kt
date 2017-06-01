package core.procedures.generic

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons
import core.scm.Type
import core.utils.Utils

class Take : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(Type.ExactNonNegativeInteger.javaClass))) {

    override val isPure = true
    override val name = "take"

    // TODO optimize
    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val count = (arg1 as Number).toInt()
        val iterator = Utils.toSequence(arg2)
        val result = Cons.list<Any?>()
        var n = 0L
        while (iterator.hasNext() && n < count) {
            result.add(iterator.next())
            n += 1
        }
        return result
    }
}
