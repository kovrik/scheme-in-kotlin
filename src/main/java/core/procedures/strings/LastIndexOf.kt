package core.procedures.strings

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.utils.Utils

class LastIndexOf : AFn(FnArgsBuilder().min(2).max(3).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name: String
        get() = "last-index-of"

    override fun apply(vararg args: Any?): Int {
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
