package core.procedures.arrays

import core.procedures.AFn

class ObjectsLength : AFn<Array<Any?>?, Long>(name = "objects-length", isPure = true, minArgs = 1, maxArgs = 1,
                                              mandatoryArgsTypes = arrayOf<Class<*>>(Array<Any?>::class.java)) {

    override operator fun invoke(arg: Array<Any?>?) = arg!!.size.toLong()
}
