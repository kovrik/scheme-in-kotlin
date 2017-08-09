package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class ListToInts : AFn<List<*>?, IntArray?>(name = "list->ints", isPure = true, minArgs = 1, maxArgs = 1,
                                            mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

    override operator fun invoke(arg: List<*>?): IntArray? {
        if (arg == null) return null
        arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Ints", arg) }
        return  (arg as? List<Int>)?.toIntArray()
    }
}
