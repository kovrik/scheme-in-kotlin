package core.scm

import core.procedures.AFn
import core.Writer
import core.procedures.Arity.Exactly
import java.util.*

/* Immutable Vector */
open class Vector : AFn<Number?, Any?>, Collection<Any?>, IAssoc<Any?, Any?> {

    companion object {
        /* Scheme Vector syntax */
//        private const val OPEN = "#("
//        private const val CLOSE = ")"
        /* Alternative Clojure-style Vector syntax */
        private const val OPEN = "["
        private const val CLOSE = "]"

        val EMPTY = Vector(0, null)
    }

    override val name = "vector"

    /* Contents of Vector: plain Java array */
    internal val array: Array<Any?>

    constructor() : super(arity = Exactly(1), mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java)) {
        array = emptyArray()
    }

    constructor(size: Int) : this(size, null)

    constructor(size: Int, init: Any?) {
        array = arrayOfNulls(size)
        array.fill(init)
    }

    constructor(coll: Collection<Any?>) : super(arity = Exactly(1),
                                                mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java)) {
        array = coll.toTypedArray()
    }

    constructor(elements: Array<out Any?>) : super(arity = Exactly(1),
                                                   mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java)) {
        array = Arrays.copyOf(elements, elements.size)
    }

    constructor(vector: Vector) : super(arity = Exactly(1),
                                        mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java)) {
        array = vector.array.copyOf()
    }

    constructor(vector: Vector, fromIndex: Int, toIndex: Int) : super(arity = Exactly(1),
                                              mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java)) {
        array = vector.array.copyOfRange(fromIndex, toIndex)
    }

    override val size: Int
        get() = array.size

    override fun contains(element: Any?) = element in array

    operator fun get(index: Int) = array[index]

    override operator fun invoke(arg: Number?) = arg!!.toInt().let {
        when {
            it < array.size -> array[it]
            else -> throw IndexOutOfBoundsException("$name: value out of range: $it")
        }
    }

    override fun toString() = joinToString(prefix = OPEN, separator = " ", postfix = CLOSE, transform = Writer::write)

    override fun iterator() = array.asList().iterator()

    override fun isEmpty() = (size == 0)

    open fun getArray(): Array<Any?> = array.copyOf()

    open fun toArray(): Array<Any?> = array.copyOf()

    override fun containsAll(elements: Collection<*>) = elements.all { contains(it) }

    override fun hashCode() = array.hashCode()

    override fun equals(other: Any?) = other is Vector && Arrays.equals(array, other.array)

    override fun containsKey(key: Any?) = when (key) {
        is Number -> size > key.toInt()
        else -> false
    }

    override fun getEntry(key: Any?): MapEntry<Int, Any?>? = when (key) {
        is Number -> array.getOrNull(key.toInt())?.let { MapEntry<Int, Any?>(key.toInt(), it) }
        else -> null
    }

    override fun assoc(key: Any?, value: Any?): Any = throw UnsupportedOperationException("assoc is not supported for immutable vector")
}
