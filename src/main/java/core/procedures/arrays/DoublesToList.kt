package core.procedures.arrays

import core.procedures.AFn

class DoublesToList : AFn<DoubleArray?, List<Double>?>(name = "doubles->list", isPure = true, minArgs = 1, maxArgs = 1,
                                                       mandatoryArgsTypes = arrayOf<Class<*>>(DoubleArray::class.java)) {

    override operator fun invoke(arg: DoubleArray?) = arg?.toList()
}
