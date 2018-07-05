package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutableString

class StringFill : AFn<Any?, MutableString>(name = "string-fill!", arity = Exactly(2),
                                            mandatoryArgsTypes = arrayOf(MutableString::class.java,
                                                                         Char::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): MutableString {
        val s = arg1 as MutableString?
        val oldLength = s!!.length
        return s.apply {
            clear()
            for (i in 0 until oldLength) { append(arg2) }
        }
    }
}
