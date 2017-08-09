package core.procedures.arrays

import core.procedures.AFn

class BooleansToList : AFn<BooleanArray?, List<Boolean>?>(name = "booleans->list", isPure = true, minArgs = 1, maxArgs = 1,
                                                 mandatoryArgsTypes = arrayOf<Class<*>>(BooleanArray::class.java)) {

    override operator fun invoke(arg: BooleanArray?) = arg?.toList()
}
