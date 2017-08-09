package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class ListToObjects : AFn<List<*>?, Array<*>?>(name = "list->objects", isPure = true, minArgs = 1, maxArgs = 1,
                                               mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

    override operator fun invoke(arg: List<*>?): Array<*>? {
        if (arg == null) return null
        arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Objects", arg) }
        return  (arg as? List<*>)?.toTypedArray()
    }
}
