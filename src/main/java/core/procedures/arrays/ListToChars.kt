package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class ListToChars : AFn<List<*>?, CharArray?>(name = "list->chars", isPure = true, minArgs = 1, maxArgs = 1,
                                              mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

    override operator fun invoke(arg: List<*>?): CharArray? {
        if (arg == null) return null
        arg.filterNot { Utils.isChar(it) }.forEach { throw WrongTypeException(name, "List of Chars", arg) }
        return  (arg as? List<Char>)?.toCharArray()
    }
}
