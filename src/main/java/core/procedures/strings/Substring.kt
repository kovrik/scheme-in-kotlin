package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

open class Substring : AFn(FnArgsBuilder().min(2).max(3)
        .mandatory(arrayOf(CharSequence::class.java, Type.ExactNonNegativeInteger::class.java))
        .rest(Type.ExactNonNegativeInteger::class.java).build()) {

    override val name = "substring"

    override operator fun invoke(vararg args: Any?): String? {
        val s = args[0].toString()
        val start = (args[1] as Number).toLong()
        if (start > s.length) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = s.length.toLong()
        if (args.size == 3) {
            end = args[2] as Long
        }
        if (end > s.length) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return s.substring(start.toInt(), end.toInt())
    }
}
