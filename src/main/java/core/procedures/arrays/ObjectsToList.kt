package core.procedures.arrays

import core.procedures.AFn

class ObjectsToList : AFn<Array<*>?, List<Any?>?>(name = "objects->list", isPure = true, minArgs = 1, maxArgs = 1,
                                                    mandatoryArgsTypes = arrayOf<Class<*>>(Array<Any?>::class.java)) {

    override operator fun invoke(arg: Array<*>?) = arg?.toList()
}
