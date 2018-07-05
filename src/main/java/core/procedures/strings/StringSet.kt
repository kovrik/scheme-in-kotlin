package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutableString
import core.scm.Type

class StringSet : AFn<Any?, Unit>(name = "string-set!", arity = Exactly(3),
                                  mandatoryArgsTypes = arrayOf(MutableString::class.java,
                                                               Type.ExactNonNegativeInteger::class.java,
                                                               Char::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
        (arg1!! as MutableString)[(arg2 as Number).toInt()] = (arg3!! as Char)
    }
}
