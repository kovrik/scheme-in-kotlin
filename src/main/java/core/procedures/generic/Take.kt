package core.procedures.generic

import core.procedures.AFn
import core.scm.Cons
import core.utils.Utils

class Take : AFn(name = "take", isPure = true, minArgs = 2, maxArgs = 2, mandatoryArgsTypes = arrayOf(Int::class.javaObjectType)) {

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
