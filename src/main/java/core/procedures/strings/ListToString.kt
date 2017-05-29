package core.procedures.strings

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Type

class ListToString : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.ProperList::class.java))) {

    override val name = "list->string"

    override operator fun invoke(arg: Any?): Any {
        val cs = arg!! as List<*>
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
