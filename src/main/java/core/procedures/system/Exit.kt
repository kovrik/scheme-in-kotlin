package core.procedures.system

import core.procedures.AFn

class Exit : AFn(name = "exit", maxArgs = 1, restArgsType = Long::class.java) {

    override operator fun invoke(vararg args: Any?) = when {
        args.isEmpty() -> System.exit(0)
        else -> System.exit((args[0] as Number).toInt())
    }
}
