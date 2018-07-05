package core.procedures.interop

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.Exactly

class CharType : AFn<Any?, Char>(name = "char", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Number -> arg.toInt().toChar()
        is Char   -> arg
        else      -> throw WrongTypeException(name, "Character or Number", arg)
    }
}
