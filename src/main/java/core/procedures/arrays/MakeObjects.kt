package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class MakeObjects : AFn<Any?, Array<*>>(name = "make-objects", isPure = true, minArgs = 1, maxArgs = 2,
                                        mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java),
                                        lastArgType = Any::class.java) {

    override operator fun invoke(args: Array<out Any?>): Array<*> {
        val length = (args[0] as Number).toInt()
        val obj = if (args.size == 1) null else args[1]
        return arrayOfNulls<Any?>(length).apply { for (i in 0..length - 1) { set(i, obj) } }
    }
}
