package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

object ListToArrays {

    class ListToBooleans : AFn<List<*>?, BooleanArray?>(name = "list->booleans", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

        override operator fun invoke(arg: List<*>?): BooleanArray? {
            if (arg == null) return null
            arg.filterNot { it is Boolean }.forEach { throw WrongTypeException(name, "List of Booleans", arg) }
            return  (arg as? List<Boolean>)?.toBooleanArray()
        }
    }

    class ListToBytes : AFn<List<*>?, ByteArray?>(name = "list->bytes", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

        override operator fun invoke(arg: List<*>?): ByteArray? {
            if (arg == null) return null
            arg.filterNot { Utils.isByte(it) }.forEach { throw WrongTypeException(name, "List of Bytes", arg) }
            return  (arg as? List<Byte>)?.toByteArray()
        }
    }

    class ListToChars : AFn<List<*>?, CharArray?>(name = "list->chars", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

        override operator fun invoke(arg: List<*>?): CharArray? {
            if (arg == null) return null
            arg.filterNot { Utils.isChar(it) }.forEach { throw WrongTypeException(name, "List of Chars", arg) }
            return  (arg as? List<Char>)?.toCharArray()
        }
    }

    class ListToDoubles : AFn<List<*>?, DoubleArray?>(name = "list->doubles", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

        override operator fun invoke(arg: List<*>?): DoubleArray? {
            if (arg == null) return null
            arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Doubles", arg) }
            return  (arg as? List<Double>)?.toDoubleArray()
        }
    }

    class ListToFloats : AFn<List<*>?, FloatArray?>(name = "list->floats", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

        override operator fun invoke(arg: List<*>?): FloatArray? {
            if (arg == null) return null
            arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Floats", arg) }
            return  (arg as? List<Float>)?.toFloatArray()
        }
    }

    class ListToInts : AFn<List<*>?, IntArray?>(name = "list->ints", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

        override operator fun invoke(arg: List<*>?): IntArray? {
            if (arg == null) return null
            arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Ints", arg) }
            return  (arg as? List<Int>)?.toIntArray()
        }
    }

    class ListToLongs : AFn<List<*>?, LongArray?>(name = "list->longs", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

        override operator fun invoke(arg: List<*>?): LongArray? {
            if (arg == null) return null
            arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Longs", arg) }
            return  (arg as? List<Long>)?.toLongArray()
        }
    }

    class ListToObjects : AFn<List<*>?, Array<*>?>(name = "list->objects", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

        override operator fun invoke(arg: List<*>?): Array<*>? {
            if (arg == null) return null
            arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Objects", arg) }
            return  (arg as? List<*>)?.toTypedArray()
        }
    }

    class ListToShorts : AFn<List<*>?, ShortArray?>(name = "list->shorts", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

        override operator fun invoke(arg: List<*>?): ShortArray? {
            if (arg == null) return null
            arg.filterNot(Utils::isReal).forEach { throw WrongTypeException(name, "List of Shorts", arg) }
            return  (arg as? List<Short>)?.toShortArray()
        }
    }
}