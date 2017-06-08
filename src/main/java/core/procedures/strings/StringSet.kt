package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableString
import core.scm.Type

class StringSet : AFn(FnArgs(min = 3, max = 3, mandatory = arrayOf(MutableString::class.java, Type.ExactNonNegativeInteger::class.java, Char::class.javaObjectType))) {

    override val name = "string-set!"

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
        (arg1!! as MutableString)[(arg2 as Number).toInt()] = (arg3!! as Char)
    }
}
