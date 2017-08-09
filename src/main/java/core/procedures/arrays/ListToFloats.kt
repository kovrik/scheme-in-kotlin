package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class ListToFloats : AFn<List<*>?, FloatArray?>(name = "list->floats", isPure = true, minArgs = 1, maxArgs = 1,
                                                mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

    override operator fun invoke(arg: List<*>?): FloatArray? {
        if (arg == null) return null
        arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Floats", arg) }
        return  (arg as? List<Float>)?.toFloatArray()
    }
}
