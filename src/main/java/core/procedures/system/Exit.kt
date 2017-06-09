package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs

class Exit : AFn(FnArgs(max = 1, rest = Long::class.java)) {

    override val name = "exit"

    override operator fun invoke(vararg args: Any?) = when {
        args.isEmpty() -> System.exit(0)
        else -> System.exit((args[0] as Number).toInt())
    }
}
