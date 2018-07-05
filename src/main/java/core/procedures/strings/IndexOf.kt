package core.procedures.strings

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.Type

class IndexOf : AFn<Any?, Int>(name = "index-of", isPure = true, arity = Range(2, 3),
                               mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args[1] !is CharSequence && args[1] !is Char -> throw WrongTypeException(name, "String or Character", args[1])
        args.size == 3 -> {
            val index = args[2]
            Type.assertType(name, index, Type.Real::class.java)
            when (args[1]) {
                is Char -> args[0].toString().indexOf(args[1] as Char, (index as Number).toInt())
                else    -> args[0].toString().indexOf(args[1].toString(), (index as Number).toInt())
            }
        }
        args[1] is Char -> args[0].toString().indexOf((args[1] as Char))
        else -> args[0].toString().indexOf(args[1].toString())
    }
}
