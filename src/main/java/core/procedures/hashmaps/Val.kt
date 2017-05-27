package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Val : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Map.Entry::class.java)).build()) {

    override val isPure = true
    override val name = "val"

    override operator fun invoke(arg: Any?): Any? {
        return (arg as Map.Entry<*, *>).value
    }
}
