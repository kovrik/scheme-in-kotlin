package core.procedures.exceptions

import core.exceptions.ExInfoException
import core.procedures.AFn

class ExInfo : AFn<Any?, ExInfoException>(name = "ex-info", isPure = true, minArgs = 2, maxArgs = 3,
                   mandatoryArgsTypes = arrayOf(String::class.java, Map::class.java), restArgsType = Throwable::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size == 2 -> ExInfoException(args[0] as String, args[1] as Map<*, *>)
        else           -> ExInfoException(args[0] as String, args[1] as Map<*, *>, args[2] as Throwable)
    }
}
