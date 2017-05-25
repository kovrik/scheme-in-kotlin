package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.MutableString
import core.scm.Type
import core.scm.Void

class StringSet : AFn(FnArgsBuilder().min(3).max(3).mandatory(arrayOf(MutableString::class.java, Type.ExactNonNegativeInteger::class.java, Char::class.javaObjectType)).build()) {

    override val name: String
        get() = "string-set!"

    override val isPure: Boolean
        get() = false

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): Any? {
        val str = arg1 as MutableString?
        val pos = (arg2 as Number).toLong()
        if (pos >= str!!.length) {
            throw IndexOutOfBoundsException(String.format("%s: value out of range: %s", name, pos))
        }
        str.setCharAt(pos.toInt(), (arg3 as Char?)!!)
        return Void.VOID
    }
}
