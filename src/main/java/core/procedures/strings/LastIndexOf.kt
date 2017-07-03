package core.procedures.strings

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class LastIndexOf : AFn<Any?, Int>(name = "last-index-of", isPure = true, minArgs =  2, maxArgs = 3,
                        mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(args: Array<out Any?>): Int {
        if (args[1] !is CharSequence && args[1] !is Char) {
            throw WrongTypeException(name, "String or Character", args[1])
        }
        if (args.size == 3) {
            val index = args[2]
            if (!Utils.isReal(index)) {
                throw WrongTypeException(name, "Real", index)
            }
            if (args[1] is Char) {
                return args[0].toString().lastIndexOf((args[1] as Char), (index as Number).toInt())
            }
            return args[0].toString().lastIndexOf(args[1].toString(), (index as Number).toInt())
        }
        if (args[1] is Char) {
            return args[0].toString().lastIndexOf((args[1] as Char))
        }
        return args[0].toString().lastIndexOf(args[1].toString())
    }
}
