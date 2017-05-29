package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons

class StringToList : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(CharSequence::class.java))) {

    override val name = "string->list"

    override operator fun invoke(arg: Any?): Cons<Char?>? {
        val list = Cons.list<Char>()
        for (c in arg!!.toString().toCharArray()) {
            list.add(c)
        }
        return list
    }
}