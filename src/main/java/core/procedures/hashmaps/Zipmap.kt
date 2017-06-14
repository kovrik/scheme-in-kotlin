package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.InvokableMap
import core.utils.Utils

class Zipmap : AFn<Any?, Map<*, *>>(name = "zipmap", isPure = true, minArgs = 2, maxArgs = 2) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Map<*, *> {
        val iterator1 = Utils.toSequence(arg1)
        val iterator2 = Utils.toSequence(arg2)
        val map = InvokableMap()
        while (iterator1.hasNext() && iterator2.hasNext()) {
            map.put(iterator1.next(), iterator2.next())
        }
        return map
    }
}
