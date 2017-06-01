package core.scm

import core.exceptions.WrongTypeException
import core.writer.Writer
import java.util.*

class Cons<E> : LinkedList<E?> {

    /* Is it a Proper List or an Improper one?
     * Proper lists just have this flag set to true, they don't end with empty list.
     **/
    var isProperList = true

    private constructor() : super()
    private constructor(c: Collection<E>) : super(c)
    private constructor(car: E?, cdr: E?) : super() {
        add(car)
        isProperList = isProperList(cdr)
        when {
            isProperList -> addAll(cdr as List<E>)
            else         -> add(cdr)
        }
    }

    fun car() = when {
        isEmpty() -> throw WrongTypeException("car", Type.Pair::class.java, EMPTY)
        else      -> first
    }

    fun cdr() = if (isProperList) subList(1, size) else (last as Any)

    override fun toString() = toString(this)

    override fun hashCode() = 31 * super.hashCode() + if (isProperList) 1 else 0

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null) return false
        if (other !is List<*>) return false
        /* Two empty lists are always equal */
        if (size == 0 && other.size == 0) return true
        if (this.size != other.size) return false
        /* Improper lists are not equal to Proper lists, even if they have the same elements */
        if (other is Cons<*> && isProperList != other.isProperList) return false
        val thisIterator = this.iterator()
        val otherIterator = other.iterator()
        while (thisIterator.hasNext() && otherIterator.hasNext()) {
            if (thisIterator.next() != otherIterator.next()) return false
        }
        return true
    }

    companion object {

        /* Empty list constant */
        val EMPTY = Cons<Any?>()

        fun <E> cons(car: E?, cdr: E?): Cons<E> {
            if (cdr == null) return Cons(car, EMPTY) as Cons<E>
            return Cons(car, cdr)
        }

        fun <E> list(): Cons<E?> = Cons()

        fun <E> list(e: E?): Cons<E?> {
            val list = list<E?>()
            list.add(e)
            return list
        }

        fun <E> list(e1: E?, e2: E?): Cons<E?> {
            val list = list<E?>(e1)
            list.add(e2)
            return list
        }

        fun <E> list(e1: E?, e2: E?, e3: E?): Cons<E?> {
            val list = list<E?>(e1, e2)
            list.add(e3)
            return list
        }

        fun <E> list(vararg elements: E?) = if (elements.isEmpty()) EMPTY as Cons<E?> else list(Arrays.asList(*elements))

        fun <E> list(c: Collection<E?>) = if (c.isEmpty()) EMPTY as Cons<E?> else Cons(c)

        /* Return true if o is a Proper List */
        fun isProperList(o: Any?) = o is List<*> && o !is Cons<*> || o is Cons<*> && o.isProperList

        fun isPair(o: Any?) = o is List<*> && !o.isEmpty()

        /* Use this method to print all lists */
        fun toString(list: List<*>): String {
            if (list.isEmpty()) {
                return "()"
            }
            /* Cons cell */
            val sb = StringBuilder("(")
            if (isProperList(list)) {
                /* List */
                for (i in 0..list.size - 1 - 1) {
                    val e = list[i]
                    sb.append(if (e === list) "(this list)" else Writer.write(e)).append(' ')
                }
                sb.append(Writer.write(list[list.size - 1]))
            } else {
                sb.append(Writer.write(list[0]))
                var cdr = list[list.size - 1]
                while (cdr is Cons<*>) {
                    sb.append(" ").append(Writer.write(cdr.first))
                    cdr = cdr.last
                }
                /* Dotted notation */
                sb.append(" . ").append(Writer.write(cdr))
            }
            return sb.append(')').toString()
        }
    }
}
