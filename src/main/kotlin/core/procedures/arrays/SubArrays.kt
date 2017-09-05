package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

object SubArrays {

    open class Subbooleans : AFn<Any?, BooleanArray>(name = "subbooleans", isPure = true, minArgs = 2, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(BooleanArray::class.java, Type.ExactNonNegativeInteger::class.java),
            restArgsType = Type.ExactNonNegativeInteger::class.java) {

        override operator fun invoke(args: Array<out Any?>): BooleanArray {
            val booleans = args[0] as BooleanArray
            val start = (args[1] as Number).toInt()
            if (start > booleans.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $start")
            }
            var end = booleans.size
            if (args.size == 3) {
                end = (args[2] as Number).toInt()
            }
            if (end > booleans.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $end")
            }
            return booleans.copyOfRange(start, end)
        }
    }

    open class Subbytes : AFn<Any?, ByteArray>(name = "subbytes", isPure = true, minArgs = 2, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(ByteArray::class.java, Type.ExactNonNegativeInteger::class.java),
            restArgsType = Type.ExactNonNegativeInteger::class.java) {

        override operator fun invoke(args: Array<out Any?>): ByteArray {
            val bytes = args[0] as ByteArray
            val start = (args[1] as Number).toInt()
            if (start > bytes.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $start")
            }
            var end = bytes.size
            if (args.size == 3) {
                end = (args[2] as Number).toInt()
            }
            if (end > bytes.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $end")
            }
            return bytes.copyOfRange(start, end)
        }
    }

    open class Subchars : AFn<Any?, CharArray>(name = "subchars", isPure = true, minArgs = 2, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(CharArray::class.java, Type.ExactNonNegativeInteger::class.java),
            restArgsType = Type.ExactNonNegativeInteger::class.java) {

        override operator fun invoke(args: Array<out Any?>): CharArray {
            val chars = args[0] as CharArray
            val start = (args[1] as Number).toInt()
            if (start > chars.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $start")
            }
            var end = chars.size
            if (args.size == 3) {
                end = (args[2] as Number).toInt()
            }
            if (end > chars.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $end")
            }
            return chars.copyOfRange(start, end)
        }
    }

    open class Subdoubles : AFn<Any?, DoubleArray>(name = "subdoubles", isPure = true, minArgs = 2, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(DoubleArray::class.java, Type.ExactNonNegativeInteger::class.java),
            restArgsType = Type.ExactNonNegativeInteger::class.java) {

        override operator fun invoke(args: Array<out Any?>): DoubleArray {
            val doubles = args[0] as DoubleArray
            val start = (args[1] as Number).toInt()
            if (start > doubles.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $start")
            }
            var end = doubles.size
            if (args.size == 3) {
                end = (args[2] as Number).toInt()
            }
            if (end > doubles.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $end")
            }
            return doubles.copyOfRange(start, end)
        }
    }

    open class Subfloats : AFn<Any?, FloatArray>(name = "subfloats", isPure = true, minArgs = 2, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(FloatArray::class.java, Type.ExactNonNegativeInteger::class.java),
            restArgsType = Type.ExactNonNegativeInteger::class.java) {

        override operator fun invoke(args: Array<out Any?>): FloatArray {
            val floats = args[0] as FloatArray
            val start = (args[1] as Number).toInt()
            if (start > floats.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $start")
            }
            var end = floats.size
            if (args.size == 3) {
                end = (args[2] as Number).toInt()
            }
            if (end > floats.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $end")
            }
            return floats.copyOfRange(start, end)
        }
    }

    open class Subints : AFn<Any?, IntArray>(name = "subints", isPure = true, minArgs = 2, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(IntArray::class.java, Type.ExactNonNegativeInteger::class.java),
            restArgsType = Type.ExactNonNegativeInteger::class.java) {

        override operator fun invoke(args: Array<out Any?>): IntArray {
            val ints = args[0] as IntArray
            val start = (args[1] as Number).toInt()
            if (start > ints.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $start")
            }
            var end = ints.size
            if (args.size == 3) {
                end = (args[2] as Number).toInt()
            }
            if (end > ints.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $end")
            }
            return ints.copyOfRange(start, end)
        }
    }

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

    open class Subobjects : AFn<Any?, Array<*>>(name = "subobjects", isPure = true, minArgs = 2, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(Array<Any?>::class.java, Type.ExactNonNegativeInteger::class.java),
            restArgsType = Type.ExactNonNegativeInteger::class.java) {

        override operator fun invoke(args: Array<out Any?>): Array<*> {
            val objects = args[0] as Array<*>
            val start = (args[1] as Number).toInt()
            if (start > objects.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $start")
            }
            var end = objects.size
            if (args.size == 3) {
                end = (args[2] as Number).toInt()
            }
            if (end > objects.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $end")
            }
            return objects.copyOfRange(start, end)
        }
    }

    open class Subshorts : AFn<Any?, ShortArray>(name = "subshorts", isPure = true, minArgs = 2, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(ShortArray::class.java, Type.ExactNonNegativeInteger::class.java),
            restArgsType = Type.ExactNonNegativeInteger::class.java) {

        override operator fun invoke(args: Array<out Any?>): ShortArray {
            val shorts = args[0] as ShortArray
            val start = (args[1] as Number).toInt()
            if (start > shorts.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $start")
            }
            var end = shorts.size
            if (args.size == 3) {
                end = (args[2] as Number).toInt()
            }
            if (end > shorts.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $end")
            }
            return shorts.copyOfRange(start, end)
        }
    }
}