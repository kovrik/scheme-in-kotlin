package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgs
import core.utils.Utils

class Zipmap : AFn(FnArgs(min = 2, max = 2)) {

    override val isPure = true
    override val name = "zipmap"

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val iterator1 = Utils.toSequence(arg1)
        val iterator2 = Utils.toSequence(arg2)
        val map = HashMap<Any?, Any?>()
        while (iterator1.hasNext() && iterator2.hasNext()) {
            map.put(iterator1.next(), iterator2.next())
        }
        return map
    }
}
