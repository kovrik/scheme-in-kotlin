package core.procedures.arrays

import core.procedures.AFn

class IntsLength : AFn<IntArray?, Long>(name = "ints-length", isPure = true, minArgs = 1, maxArgs = 1,
                                        mandatoryArgsTypes = arrayOf<Class<*>>(IntArray::class.java)) {

    override operator fun invoke(arg: IntArray?) = arg!!.size.toLong()
}
