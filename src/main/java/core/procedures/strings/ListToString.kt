package core.procedures.strings

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Type

class ListToString : AFn<List<*>?, String>(name = "list->string", isPure = true, minArgs = 1, maxArgs = 1,
                         mandatoryArgsTypes = arrayOf<Class<*>>(Type.ProperList::class.java)) {

    override operator fun invoke(arg: List<*>?): String {
        val cs = arg!!
        if (cs.isEmpty()) {
            return ""
        }
        val sb = StringBuilder(cs.size)
        for (c in cs) {
            if (c !is Char) throw WrongTypeException(name, "Character", c)
            sb.append(c)
        }
        return sb.toString()
    }
}
