package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Key : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Map.Entry::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "key"

    override fun apply1(arg: Any?): Any? {
        return (arg as Map.Entry<*, *>).key
    }
}
