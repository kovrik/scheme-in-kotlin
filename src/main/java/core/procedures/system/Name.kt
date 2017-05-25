package core.procedures.system

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.INamed

class Name : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "name"

    override operator fun invoke(arg: Any?): CharSequence? {
        when (arg) {
            is INamed -> return arg.name
            is CharSequence -> return arg
            else -> throw WrongTypeException(name, "String or Symbol or Keyword", arg)
        }
    }
}
