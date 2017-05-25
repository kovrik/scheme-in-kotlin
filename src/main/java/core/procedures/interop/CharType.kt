package core.procedures.interop

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder

class CharType : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "char"

    override operator fun invoke(arg: Any?): Char? {
        /* Have to box it */
        when (arg) {
            is Number -> return arg.toInt().toChar()
            is Char -> return arg
            else -> throw WrongTypeException("char", "Character or Number", arg)
        }
    }
}
