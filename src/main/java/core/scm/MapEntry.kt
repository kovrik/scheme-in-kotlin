package core.scm

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.writer.Writer

class MapEntry<K, V>(override var key: K, override var value: V) :
        AFn<Number?, Any?>(minArgs = 1, maxArgs = 1, isPure = true, mandatoryArgsTypes = arrayOf(Int::class.java)),
        Map.Entry<K, V>, IAssoc<Number?, Any?>, Sequence<Any?> {

    constructor(entry: Map.Entry<K, V>) : this(entry.key, entry.value)

    override fun containsKey(key: Number?) = when {
        key == null -> false
        key.toInt() == 0 -> true
        key.toInt() == 1 -> true
        else -> false
    }

    override fun getEntry(key: Number?) = when {
        key == null -> null
        key.toInt() == 0 -> MapEntry(0, this.key)
        key.toInt() == 1 -> MapEntry(1, this.value)
        else -> null
    }

    override fun assoc(key: Number?, value: Any?): MapEntry<*, *> {
        Type.assertType("Map Entry", key, Int::class.java)
        return when {
            key == null -> throw WrongTypeException("Map Entry", "Integer", key)
            key.toInt() == 0 -> MapEntry(value, this.value)
            key.toInt() == 1 -> MapEntry(this.key, value)
            else -> throw IndexOutOfBoundsException()
        }
    }

    override operator fun invoke(arg: Number?) = getEntry(arg)

    override fun iterator() = sequenceOf(key, value).iterator()

    override fun toString() = "[${Writer.write(key)} ${Writer.write(value)}]"

    override fun equals(other: Any?) = when {
        this === other -> true
        other == null || javaClass != other.javaClass -> false
        else -> key == (other as MapEntry<*, *>).key && value == other.value
    }

    override fun hashCode() = 31 * (key?.hashCode() ?: 0) + (value?.hashCode() ?: 0)
}
