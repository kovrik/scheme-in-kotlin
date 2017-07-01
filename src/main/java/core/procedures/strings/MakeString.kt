package core.procedures.strings

import core.procedures.AFn
import core.scm.MutableString
import core.scm.Type

class MakeString : AFn<Any?, MutableString>(name = "make-string", isPure = true, minArgs = 1, maxArgs = 2,
                       mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java),
                       restArgsType = Char::class.javaObjectType) {

    override operator fun invoke(args: Array<Any?>): MutableString {
        val s = (args[0] as Number).toInt()
        val c = if (args.size == 1) Character.MIN_VALUE else args[1]
        val string = MutableString(s)
        for (i in 0..s - 1) {
            string.append(c)
        }
        return string
    }
}
