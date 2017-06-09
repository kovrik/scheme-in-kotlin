package core.procedures.strings

import core.procedures.AFn
import core.scm.Cons

class StringToList : AFn(name = "string->list", isPure = true, minArgs = 1, maxArgs = 1,
                         mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?): Cons<Char?>? {
        val list = Cons.list<Char>()
        for (c in arg!!.toString().toCharArray()) {
            list.add(c)
        }
        return list
    }
}