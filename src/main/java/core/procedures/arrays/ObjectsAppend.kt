package core.procedures.arrays

import core.procedures.AFn

class ObjectsAppend : AFn<Any?, Array<*>>(name = "objects-append", isPure = true, restArgsType = Array<Any?>::class.java) {

    override operator fun invoke(args: Array<out Any?>): Array<*> {
        val objects = mutableListOf<Any?>()
        for (arr in args) {
            for (b in (arr as Array<Any?>)) {
                objects.add(b)
            }
        }
        return objects.toTypedArray()
    }
}
