package core.procedures.strings

import core.procedures.AFn
import core.scm.MutableString

class StringFill : AFn(name = "string-fill!", minArgs = 2, maxArgs = 2,
                       mandatoryArgsTypes = arrayOf(MutableString::class.java, Char::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): MutableString? {
        val s = arg1 as MutableString?
        val oldLength = s!!.length
        s.clear()
        for (i in 0..oldLength - 1) {
            s.append(arg2)
        }
        return s
    }
}
