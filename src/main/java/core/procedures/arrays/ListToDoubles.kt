package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class ListToDoubles : AFn<List<*>?, DoubleArray?>(name = "list->doubles", isPure = true, minArgs = 1, maxArgs = 1,
                                                  mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

    override operator fun invoke(arg: List<*>?): DoubleArray? {
        if (arg == null) return null
        arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Doubles", arg) }
        return  (arg as? List<Double>)?.toDoubleArray()
    }
}
