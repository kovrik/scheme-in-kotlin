package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons

class StringToList : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name: String
        get() = "string->list"

    override fun apply1(arg: Any?): Cons<Char>? {
        val list = Cons.list<Char>()
        for (c in arg!!.toString().toCharArray()) {
            list.add(c)
        }
        return list
    }
}