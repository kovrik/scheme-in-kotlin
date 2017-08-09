package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn

class ListToBooleans : AFn<List<*>?, BooleanArray?>(name = "list->booleans", isPure = true, minArgs = 1, maxArgs = 1,
                                                    mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

    override operator fun invoke(arg: List<*>?): BooleanArray? {
        if (arg == null) return null
        arg.filterNot { it is Boolean }.forEach { throw WrongTypeException(name, "List of Booleans", arg) }
        return  (arg as? List<Boolean>)?.toBooleanArray()
    }
}
