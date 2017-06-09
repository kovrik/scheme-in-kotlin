package core.procedures.exceptions

import core.exceptions.ExInfoException
import core.procedures.AFn

class ExInfo : AFn(name = "ex-info", isPure = true, minArgs = 2, maxArgs = 3,
                   mandatoryArgsTypes = arrayOf(String::class.java, Map::class.java), restArgsType = Throwable::class.java) {

    override operator fun invoke(vararg args: Any?): ExInfoException? {
        if (args.size == 2) {
            return ExInfoException(args[0] as String, args[1] as Map<*, *>)
        }
        return ExInfoException(args[0] as String, args[1] as Map<*, *>, args[2] as Throwable)
    }
}
