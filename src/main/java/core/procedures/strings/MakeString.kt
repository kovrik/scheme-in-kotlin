package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableString
import core.scm.Type

class MakeString : AFn(FnArgs(min = 1, max = 2, mandatory = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java), rest = Char::class.javaObjectType)) {

    override val name = "make-string"

    override operator fun invoke(vararg args: Any?): MutableString? {
        val s = (args[0] as Number).toLong()
        val c = if (args.size == 1) Character.MIN_VALUE else args[1]
        val string = MutableString()
        for (i in 0..s - 1) {
            string.append(c)
        }
        return string
    }
}
