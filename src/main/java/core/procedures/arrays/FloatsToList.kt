package core.procedures.arrays

import core.procedures.AFn

class FloatsToList : AFn<FloatArray?, List<Float>?>(name = "floats->list", isPure = true, minArgs = 1, maxArgs = 1,
                                                    mandatoryArgsTypes = arrayOf<Class<*>>(FloatArray::class.java)) {

    override operator fun invoke(arg: FloatArray?) = arg?.toList()
}
