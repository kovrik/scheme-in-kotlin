package core.procedures.interop

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs

class CharType : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "char"

    override operator fun invoke(arg: Any?): Char? {
        /* Have to box it */
        return when (arg) {
            is Number -> arg.toInt().toChar()
            is Char   -> arg
            else      -> throw WrongTypeException("char", "Character or Number", arg)
        }
    }
}
