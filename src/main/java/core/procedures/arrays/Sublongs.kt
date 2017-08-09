package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

open class Sublongs : AFn<Any?, LongArray>(name = "sublongs", isPure = true, minArgs = 2, maxArgs = 3,
                           mandatoryArgsTypes = arrayOf(LongArray::class.java, Type.ExactNonNegativeInteger::class.java),
                           restArgsType = Type.ExactNonNegativeInteger::class.java) {

    override operator fun invoke(args: Array<out Any?>): LongArray {
        val longs = args[0] as LongArray
        val start = (args[1] as Number).toInt()
        if (start > longs.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = longs.size
        if (args.size == 3) {
            end = (args[2] as Number).toInt()
        }
        if (end > longs.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return longs.copyOfRange(start, end)
    }
}
