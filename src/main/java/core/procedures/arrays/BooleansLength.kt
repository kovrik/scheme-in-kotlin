package core.procedures.arrays

import core.procedures.AFn

class BooleansLength : AFn<BooleanArray?, Long>(name = "booleans-length", isPure = true, minArgs = 1, maxArgs = 1,
                                                mandatoryArgsTypes = arrayOf<Class<*>>(BooleanArray::class.java)) {

    override operator fun invoke(arg: BooleanArray?) = arg!!.size.toLong()
}
