package core.scm

import core.writer.Writer

// TODO implement proper interfaces, refactor
class MapEntry(key: Any?, value: Any?) : MutableVector(arrayOf(key, value)), Map.Entry<Any?, Any?> {

    constructor(entry: Map.Entry<Any?, Any?>) : this(entry.key, entry.value)

    override val name = "map entry"

    override val key: Any?
        get() = get(0)

    override val value: Any?
        get() = get(1)

    override fun toString() = "[${Writer.write(key)} ${Writer.write(value)}]"

    override fun equals(other: Any?) = when {
        this === other -> true
        other == null || javaClass != other.javaClass -> false
        else -> key == (other as MapEntry).key && value == other.value
    }

    override fun hashCode() = 31 * (key?.hashCode() ?: 0) + (value?.hashCode() ?: 0)
}
