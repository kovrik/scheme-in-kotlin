package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Void

class Exit : AFn(FnArgsBuilder().max(1).rest(Long::class.java).build()) {

    override val name: String
        get() = "exit"

    override fun apply(args: Array<Any?>): Void? {
        when {
            args.isEmpty() -> System.exit(0)
            else -> System.exit((args[0] as Long).toInt())
        }
        return Void.VOID
    }
}
