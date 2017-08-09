package core.procedures.arrays

import core.procedures.AFn

class FloatsLength : AFn<FloatArray?, Long>(name = "floats-length", isPure = true, minArgs = 1, maxArgs = 1,
                                            mandatoryArgsTypes = arrayOf<Class<*>>(FloatArray::class.java)) {

    override operator fun invoke(arg: FloatArray?) = arg!!.size.toLong()
}
