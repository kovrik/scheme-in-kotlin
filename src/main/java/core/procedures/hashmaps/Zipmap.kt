package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.utils.Utils

import java.util.HashMap

class Zipmap : AFn(FnArgsBuilder().min(2).max(2).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "zipmap"

    override fun apply2(arg1: Any?, arg2: Any?): Any? {
        val iterator1 = Utils.toSequence(arg1)
        val iterator2 = Utils.toSequence(arg2)
        val map = HashMap<Any?, Any?>()
        while (iterator1.hasNext() && iterator2.hasNext()) {
            map.put(iterator1.next(), iterator2.next())
        }
        return map
    }
}