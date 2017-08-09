package core.procedures.arrays

import core.procedures.AFn

class Objects : AFn<Any?, Array<*>>(name = "objects", isPure = true, restArgsType = Any::class.javaObjectType) {

    override operator fun invoke(args: Array<out Any?>): Array<*> {
        val objects = arrayOfNulls<Any?>(args.size)
        for (i in 0..args.size - 1) {
            objects[i] = args[i]
        }
        return objects
    }
}
