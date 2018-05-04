package core.scm

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.writer.Writer

class MapEntry(override var key: Any?, override var value: Any?) :
        AFn<Number?, Any?>(minArgs = 1, maxArgs = 1, isPure = true, mandatoryArgsTypes = arrayOf(Int::class.java)),
        Map.Entry<Any?, Any?>, IAssoc<Number?, Any?>, Sequence<Any?> {

    constructor(entry: Map.Entry<Any?, Any?>) : this(entry.key, entry.value)

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

    override fun assoc(key: Number?, value: Any?): MapEntry {
        Type.assertType("Map Entry", key, Int::class.java)
        when {
            key == null -> throw WrongTypeException("Map Entry", "Integer", key)
            key.toInt() == 0 -> this.key = value
            key.toInt() == 1 -> this.value = value
            else -> throw IndexOutOfBoundsException()
        }
        return this
    }

    override operator fun invoke(arg: Number?) = getEntry(arg)

    override fun iterator() = sequenceOf(key, value).iterator()

    override fun toString() = "[${Writer.write(key)} ${Writer.write(value)}]"

    override fun equals(other: Any?) = when {
        this === other -> true
        other == null || javaClass != other.javaClass -> false
        else -> key == (other as MapEntry).key && value == other.value
    }

    override fun hashCode() = 31 * (key?.hashCode() ?: 0) + (value?.hashCode() ?: 0)
}
