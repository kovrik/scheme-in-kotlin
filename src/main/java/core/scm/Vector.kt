package core.scm

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs
import core.utils.Utils
import core.writer.Writer
import java.util.*

// TODO implement List instead?
/* Immutable Vector */
open class Vector : AFn, Collection<Any?>, IAssoc {

    companion object {
        /* Scheme Vector syntax */
        //  private static final String OPEN = "#(";
        //  private static final String CLOSE = ")";
        /* Alternative Clojure-style Vector syntax */
        private val OPEN = "["
        private val CLOSE = "]"
    }

    /* Contents of Vector: plain Java array */
    internal val array: Array<Any?>

    constructor() : super(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java))) {
        this.array = arrayOfNulls<Any>(0)
    }

    constructor(size: Int, init: Any?) : super(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java))) {
        this.array = arrayOfNulls<Any>(size)
        Arrays.fill(array, init)
    }

    constructor(vararg elements: Any?) : super(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java))) {
        this.array = elements as Array<Any?>
    }

    override val size: Int
        get() = array.size

    override fun contains(element: Any?) = array.contains(element)

    operator fun get(index: Int) = getArray()[index]

    override operator fun invoke(arg: Any?): Any? {
        val index = (arg as Number).toInt()
        if (index >= array.size) {
            throw IndexOutOfBoundsException(name + ": value out of range: " + index)
        }
        return array[index]
    }

    override val name = "vector"

    override fun toString(): String {
        if (getArray().isEmpty()) {
            return OPEN + CLOSE
        }
        val sb = StringBuilder()
        sb.append(OPEN)
        for (i in 0..getArray().size - 1) {
            val e = getArray()[i]
            sb.append(if (e === this) "(this vector)" else Writer.write(e))
            if (i == getArray().size - 1) {
                return sb.append(CLOSE).toString()
            }
            sb.append(' ')
        }
        return sb.toString()
    }

    override fun iterator() = Arrays.asList(*array).iterator()

    override fun isEmpty() = (size == 0)

    open fun getArray(): Array<Any?> = Arrays.copyOf(array, size)

    open fun toArray(): Array<Any?> = Arrays.copyOf(array, size)

    override fun containsAll(elements: Collection<*>) = elements.any { contains(it) }

    override fun hashCode() = Arrays.hashCode(getArray())

    override fun equals(other: Any?): Boolean {
        return other is Vector && Arrays.equals(getArray(), other.getArray())
    }

    override fun containsKey(key: Any): Boolean {
        if (!Utils.isInteger(key)) {
            throw WrongTypeException(name, Int::class.java, key)
        }
        val i = (key as Number).toInt()
        return size > i
    }

    override fun getEntry(key: Any): MapEntry? {
        if (!Utils.isInteger(key)) {
            throw WrongTypeException(name, Int::class.java, key)
        }
        val i = (key as Number).toInt()
        return MapEntry(i, get(i))
    }

    override fun assoc(key: Any, value: Any): Any = throw UnsupportedOperationException("assoc is not supported for immutable vector")
}
