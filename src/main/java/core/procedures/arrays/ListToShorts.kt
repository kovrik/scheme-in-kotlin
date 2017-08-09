package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class ListToShorts : AFn<List<*>?, ShortArray?>(name = "list->shorts", isPure = true, minArgs = 1, maxArgs = 1,
                                                mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

    override operator fun invoke(arg: List<*>?): ShortArray? {
        if (arg == null) return null
        arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Shorts", arg) }
        return  (arg as? List<Short>)?.toShortArray()
    }
}
