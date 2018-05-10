package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

object ListToArrays {

    class ListToBooleans : AFn<List<*>?, BooleanArray?>(name = "list->booleans", isPure = true, minArgs = 1, maxArgs = 1,
                                                        mandatoryArgsTypes = arrayOf(List::class.java)) {

        override operator fun invoke(arg: List<*>?) = when {
            arg == null -> null
            arg.any { it !is Boolean } -> throw WrongTypeException(name, "List of Booleans", arg)
            else -> (arg as? List<Boolean>)?.toBooleanArray()
        }
    }

    class ListToBytes : AFn<List<*>?, ByteArray?>(name = "list->bytes", isPure = true, minArgs = 1, maxArgs = 1,
                                                  mandatoryArgsTypes = arrayOf(List::class.java)) {

        override operator fun invoke(arg: List<*>?) = when {
            arg == null -> null
            arg.any { !Utils.isByte(it) } -> throw WrongTypeException(name, "List of Bytes", arg)
            else -> (arg as? List<Byte>)?.toByteArray()
        }
    }

    class ListToChars : AFn<List<*>?, CharArray?>(name = "list->chars", isPure = true, minArgs = 1, maxArgs = 1,
                                                  mandatoryArgsTypes = arrayOf(List::class.java)) {

        override operator fun invoke(arg: List<*>?) = when {
            arg == null -> null
            arg.any { !Utils.isChar(it) } -> throw WrongTypeException(name, "List of Chars", arg)
            else -> (arg as? List<Char>)?.toCharArray()
        }
    }

    class ListToDoubles : AFn<List<*>?, DoubleArray?>(name = "list->doubles", isPure = true, minArgs = 1, maxArgs = 1,
                                                      mandatoryArgsTypes = arrayOf(List::class.java)) {

        override operator fun invoke(arg: List<*>?) = when {
            arg == null -> null
            arg.any { !Utils.isReal(it) } -> throw WrongTypeException(name, "List of Doubles", arg)
            else -> (arg as? List<Double>)?.toDoubleArray()
        }
    }

    class ListToFloats : AFn<List<*>?, FloatArray?>(name = "list->floats", isPure = true, minArgs = 1, maxArgs = 1,
                                                    mandatoryArgsTypes = arrayOf(List::class.java)) {

        override operator fun invoke(arg: List<*>?) = when {
            arg == null -> null
            arg.any { !Utils.isReal(it)} -> throw WrongTypeException(name, "List of Floats", arg)
            else -> (arg as? List<Float>)?.toFloatArray()
        }
    }

    class ListToInts : AFn<List<*>?, IntArray?>(name = "list->ints", isPure = true, minArgs = 1, maxArgs = 1,
                                                mandatoryArgsTypes = arrayOf(List::class.java)) {

        override operator fun invoke(arg: List<*>?) = when {
            arg == null -> null
            arg.any { !Utils.isReal(it)} -> throw WrongTypeException(name, "List of Ints", arg)
            else -> (arg as? List<Int>)?.toIntArray()
        }
    }

    class ListToLongs : AFn<List<*>?, LongArray?>(name = "list->longs", isPure = true, minArgs = 1, maxArgs = 1,
                                                  mandatoryArgsTypes = arrayOf(List::class.java)) {

        override operator fun invoke(arg: List<*>?) = when {
            arg == null -> null
            arg.any{ !Utils.isReal(it)} -> throw WrongTypeException(name, "List of Longs", arg)
            else -> (arg as? List<Long>)?.toLongArray()
        }
    }

    class ListToObjects : AFn<List<*>?, Array<*>?>(name = "list->objects", isPure = true, minArgs = 1, maxArgs = 1,
                                                   mandatoryArgsTypes = arrayOf(List::class.java)) {

        override operator fun invoke(arg: List<*>?) = when (arg) {
            null -> null
            else -> (arg as? List<*>)?.toTypedArray()
        }
    }

    class ListToShorts : AFn<List<*>?, ShortArray?>(name = "list->shorts", isPure = true, minArgs = 1, maxArgs = 1,
                                                    mandatoryArgsTypes = arrayOf(List::class.java)) {

        override operator fun invoke(arg: List<*>?) = when {
            arg == null -> null
            arg.any { !Utils.isReal(it) } -> throw WrongTypeException(name, "List of Shorts", arg)
            else -> (arg as? List<Short>)?.toShortArray()
        }
    }
}