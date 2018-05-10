package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

object MakeArrays {

    class MakeBooleans : AFn<Any?, BooleanArray>(name = "make-booleans", isPure = true, minArgs = 1, maxArgs = 2,
                                                 mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                                 restArgsType = Boolean::class.java) {

        override operator fun invoke(args: Array<out Any?>) = BooleanArray((args[0] as Number).toInt(), {
            (args.getOrNull(1) as? Boolean) ?: false
        })
    }

    class MakeBytes : AFn<Any?, ByteArray>(name = "make-bytes", isPure = true, minArgs = 1, maxArgs = 2,
                                           mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                           lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>) = ByteArray((args[0] as Number).toInt(), {
            (args.getOrNull(1) as? Number)?.toByte() ?: Byte.MIN_VALUE
        })
    }

    class MakeChars : AFn<Any?, CharArray>(name = "make-chars", isPure = true, minArgs = 1, maxArgs = 2,
                                           mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                           lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>) = CharArray((args[0] as Number).toInt(), {
            (args.getOrNull(1) as? Number)?.toChar() ?: Character.MIN_VALUE
        })
    }

    class MakeDoubles : AFn<Any?, DoubleArray>(name = "make-doubles", isPure = true, minArgs = 1, maxArgs = 2,
                                               mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                               lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>) = DoubleArray((args[0] as Number).toInt(), {
            (args.getOrNull(1) as? Number)?.toDouble() ?: Double.MIN_VALUE
        })
    }

    class MakeFloats : AFn<Any?, FloatArray>(name = "make-floats", isPure = true, minArgs = 1, maxArgs = 2,
                                             mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                             lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>) = FloatArray((args[0] as Number).toInt(), {
            (args.getOrNull(1) as? Number)?.toFloat() ?: Float.MIN_VALUE
        })
    }

    class MakeInts : AFn<Any?, IntArray>(name = "make-ints", isPure = true, minArgs = 1, maxArgs = 2,
                                         mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                         lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>) = IntArray((args[0] as Number).toInt(), {
            (args.getOrNull(1) as? Number)?.toInt() ?: Int.MIN_VALUE
        })
    }

    class MakeLongs : AFn<Any?, LongArray>(name = "make-longs", isPure = true, minArgs = 1, maxArgs = 2,
                                           mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                           lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>) = LongArray((args[0] as Number).toInt(), {
            (args.getOrNull(1) as? Number)?.toLong() ?: Long.MIN_VALUE
        })
    }

    class MakeObjects : AFn<Any?, Array<*>>(name = "make-objects", isPure = true, minArgs = 1, maxArgs = 2,
                                            mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                            lastArgType = Any::class.java) {

        override operator fun invoke(args: Array<out Any?>) = Array((args[0] as Number).toInt(), {
            args.getOrNull(1)
        })
    }

    class MakeShorts : AFn<Any?, ShortArray>(name = "make-shorts", isPure = true, minArgs = 1, maxArgs = 2,
                                             mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                             lastArgType = Number::class.java) {

        override operator fun invoke(args: Array<out Any?>) = ShortArray((args[0] as Number).toInt(), {
            (args.getOrNull(1) as? Number)?.toShort() ?: Short.MIN_VALUE
        })
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