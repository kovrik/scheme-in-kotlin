package core.procedures.meta

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.IMeta
import core.scm.Symbol

class WIthMeta : AFn<Any?, Symbol>(name = "with-meta", minArgs = 2, maxArgs = 2,
                                   mandatoryArgsTypes = arrayOf(IMeta::class.java, Map::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = when (arg1) {
        is Symbol -> Symbol(arg1.name, arg2 as Map<*, *>?)
        else      -> throw WrongTypeException(name, "IMeta", arg1)
    }
}
