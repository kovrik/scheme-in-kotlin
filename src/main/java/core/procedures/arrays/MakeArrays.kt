package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

object MakeArrays {

    class MakeBooleans : AFn<Any?, BooleanArray>(name = "make-booleans", isPure = true, minArgs = 1, maxArgs = 2,
                                                 mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                                 restArgsType = Boolean::class.java) {

        override operator fun invoke(args: Array<out Any?>): BooleanArray {
            val length = (args[0] as Number).toInt()
            val boolean = if (args.size == 1) false else args[1] as Boolean
            return BooleanArray(length).apply { fill(boolean) }
        }
    }

    class MakeBytes : AFn<Any?, ByteArray>(name = "make-bytes", isPure = true, minArgs = 1, maxArgs = 2,
                                           mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                           lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>): ByteArray {
            val length = (args[0] as Number).toInt()
            val byte = if (args.size == 1) Byte.MIN_VALUE else (args[1] as Number).toByte()
            return ByteArray(length).apply { fill(byte) }
        }
    }

    class MakeChars : AFn<Any?, CharArray>(name = "make-chars", isPure = true, minArgs = 1, maxArgs = 2,
                                           mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                           lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>): CharArray {
            val length = (args[0] as Number).toInt()
            val char = if (args.size == 1) Character.MIN_VALUE else (args[1] as Number).toChar()
            return CharArray(length).apply { fill(char) }
        }
    }

    class MakeDoubles : AFn<Any?, DoubleArray>(name = "make-doubles", isPure = true, minArgs = 1, maxArgs = 2,
                                               mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                               lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>): DoubleArray {
            val length = (args[0] as Number).toInt()
            val double = if (args.size == 1) Double.MIN_VALUE else (args[1] as Number).toDouble()
            return DoubleArray(length).apply { fill(double) }
        }
    }

    class MakeFloats : AFn<Any?, FloatArray>(name = "make-floats", isPure = true, minArgs = 1, maxArgs = 2,
                                             mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                             lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>): FloatArray {
            val length = (args[0] as Number).toInt()
            val float = if (args.size == 1) Float.MIN_VALUE else (args[1] as Number).toFloat()
            return FloatArray(length).apply { fill(float) }
        }
    }

    class MakeInts : AFn<Any?, IntArray>(name = "make-ints", isPure = true, minArgs = 1, maxArgs = 2,
                                         mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                         lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>): IntArray {
            val length = (args[0] as Number).toInt()
            val int = if (args.size == 1) Int.MIN_VALUE else (args[1] as Number).toInt()
            return IntArray(length).apply { fill(int) }
        }
    }

    class MakeLongs : AFn<Any?, LongArray>(name = "make-longs", isPure = true, minArgs = 1, maxArgs = 2,
                                           mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                           lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>): LongArray {
            val length = (args[0] as Number).toInt()
            val long = if (args.size == 1) Long.MIN_VALUE else (args[1] as Number).toLong()
            return LongArray(length).apply { fill(long) }
        }
    }

    class MakeObjects : AFn<Any?, Array<*>>(name = "make-objects", isPure = true, minArgs = 1, maxArgs = 2,
                                            mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                            lastArgType = Any::class.java) {

        override operator fun invoke(args: Array<out Any?>): Array<*> {
            val length = (args[0] as Number).toInt()
            val obj = if (args.size == 1) null else args[1]
            return arrayOfNulls<Any?>(length).apply { fill(obj) }
        }
    }

    class MakeShorts : AFn<Any?, ShortArray>(name = "make-shorts", isPure = true, minArgs = 1, maxArgs = 2,
                                             mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                             lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>): ShortArray {
            val length = (args[0] as Number).toInt()
            val short = if (args.size == 1) Short.MIN_VALUE else (args[1] as Number).toShort()
            return ShortArray(length).apply { fill(short) }
        }
    }

    class MakeArray : AFn<Any?, Any>(name = "make-array", isPure = true, minArgs = 2,
                                     mandatoryArgsTypes = arrayOf(Class::class.java, Type.ExactNonNegativeInteger::class.java),
                                     restArgsType = Type.ExactNonNegativeInteger::class.java) {

        override operator fun invoke(args: Array<out Any?>): Any {
            val clazz = args[0] as Class<*>
            return when (args.size) {
                2 -> {
                    val size = (args[1] as Number).toInt()
                    when (clazz) {
                        Boolean::class.java -> BooleanArray(size)
                        Char::class.java    -> CharArray(size)
                        Byte::class.java    -> ByteArray(size)
                        Short::class.java   -> ShortArray(size)
                        Int::class.java     -> IntArray(size)
                        Float::class.java   -> FloatArray(size)
                        Long::class.java    -> LongArray(size)
                        Double::class.java  -> DoubleArray(size)
                        else                -> java.lang.reflect.Array.newInstance(clazz, size)
                    }
                }
                else -> {
                    val dims = IntArray(args.size - 1)
                    for (i in 1 until args.size) {
                        dims[i - 1] = (args[i] as Number).toInt()
                    }
                    java.lang.reflect.Array.newInstance(clazz, *dims)
                }
            }
        }
    }
}