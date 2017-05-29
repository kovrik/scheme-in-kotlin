package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Void

class Exit : AFn(FnArgs(max = 1, rest = Long::class.java)) {

    override val name = "exit"

    override operator fun invoke(vararg args: Any?): Void? {
        when {
            args.isEmpty() -> System.exit(0)
            else -> System.exit((args[0] as Long).toInt())
        }
        return Void
    }
}
