package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class ListToLongs : AFn<List<*>?, LongArray?>(name = "list->longs", isPure = true, minArgs = 1, maxArgs = 1,
                                              mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

    override operator fun invoke(arg: List<*>?): LongArray? {
        if (arg == null) return null
        arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Longs", arg) }
        return  (arg as? List<Long>)?.toLongArray()
    }
}
