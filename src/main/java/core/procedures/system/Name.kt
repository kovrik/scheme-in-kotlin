package core.procedures.system

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.INamed

class Name : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "name"

    override operator fun invoke(arg: Any?): CharSequence? {
        return when (arg) {
            is INamed -> arg.name
            is CharSequence -> arg
            else -> throw WrongTypeException(name, "String or Symbol or Keyword", arg)
        }
    }
}
