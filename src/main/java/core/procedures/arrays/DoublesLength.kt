package core.procedures.arrays

import core.procedures.AFn

class DoublesLength : AFn<DoubleArray?, Long>(name = "doubles-length", isPure = true, minArgs = 1, maxArgs = 1,
                                              mandatoryArgsTypes = arrayOf<Class<*>>(DoubleArray::class.java)) {

    override operator fun invoke(arg: DoubleArray?) = arg!!.size.toLong()
}
