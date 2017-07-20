package core.procedures.strings

import core.procedures.AFn
import core.scm.Cons

class StringToList : AFn<CharSequence?, List<Char?>>(name = "string->list", isPure = true, minArgs = 1, maxArgs = 1,
                         mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?): Cons<Char?> {
        val list = Cons.list<Char>()
        arg!!.toString().toCharArray().forEach { list.add(it) }
        return list
    }
}