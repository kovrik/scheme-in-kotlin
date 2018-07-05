package core.procedures.system

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.INamed

class Name : AFn<Any?, CharSequence>(name = "name", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is INamed -> arg.name
        is CharSequence -> arg
        else -> throw WrongTypeException(name, "String or Symbol or Keyword", arg)
    }
}
