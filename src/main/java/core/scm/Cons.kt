package core.scm

import core.exceptions.WrongTypeException
import core.writer.Writer
import java.util.*

// TODO Separate class for Proper and Improper Lists?
class Cons<E> : LinkedList<E?> {

    var isList = true

    private constructor() : super()

    private constructor(c: Collection<E>) : super(c)

    private constructor(car: E?, cdr: E?) : super() {
        add(car)
        isList = isList(cdr)
        if (isList) {
            /* cons becomes a list */
            addAll(cdr as List<E>)
        } else {
            add(cdr)
        }
    }

    fun car(): E? {
        if (isEmpty()) throw WrongTypeException("car", Type.Pair::class.java, EMPTY)
        return first
    }

    fun cdr(): Any {
        return if (isList) subList(1, size) else (last as Any)
    }

    /* Convert list to improper list (dotted pair, cons cells) */
    fun toCons(): Cons<E> {
        if (!isList) {
            return this
        }
        /* Cons backwards */
        val last = get(size - 1)
        val beforeLast = get(size - 2)
        var cons = cons(beforeLast, last)
        for (n in size - 3 downTo 0) {
            cons = cons(get(n), cons as E)
        }
        return cons
    }

    override fun toString(): String {
        return toString(this)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null) return false
        if (other !is List<*>) return false
        /* Two empty lists are always equal */
        if (size == 0 && other.size == 0) return true
        if (this.size != other.size) return false
        /* Improper lists are not equal to Proper lists, even if they have the same elements */
        if (other is Cons<*> && isList != other.isList) return false
        val thisIterator = this.iterator()
        val otherIterator = other.iterator()
        while (thisIterator.hasNext() && otherIterator.hasNext()) {
            val thisNext = thisIterator.next()
            val oNext = otherIterator.next()
            if (thisNext != oNext) {
                return false
            }
        }
        return true
    }

    override fun hashCode(): Int {
        return 31 * super.hashCode() + if (isList) 1 else 0
    }

    companion object {

        /* Empty list constant */
        val EMPTY: Cons<Any?> = Cons()

        fun <E> cons(car: E?, cdr: E?): Cons<E> {
            if (cdr == null) return Cons(car, EMPTY) as Cons<E>
            return Cons(car, cdr)
        }

        fun <E> list(): Cons<E?> {
            return Cons()
        }

        fun <E> list(e: E?): Cons<E?> {
            val list = Cons<E?>()
            list.add(e)
            return list
        }

        fun <E> list(e1: E?, e2: E?): Cons<E?> {
            val list = Cons<E?>()
            list.add(e1)
            list.add(e2)
            return list
        }

        fun <E> list(e1: E?, e2: E?, e3: E?): Cons<E?> {
            val list = Cons<E?>()
            list.add(e1)
            list.add(e2)
            list.add(e3)
            return list
        }

        fun <E> list(vararg elements: E?): Cons<E?> {
            return if (elements.isEmpty()) EMPTY as Cons<E?> else list(Arrays.asList(*elements))
        }

        fun <E> list(c: Collection<E?>): Cons<E?> {
            return if (c.isEmpty()) EMPTY as Cons<E?> else Cons(c)
        }

        /* Return true if o is a List or Cons and a list */
        fun isList(o: Any?): Boolean {
            return o is List<*> && o !is Cons<*> || o is Cons<*> && o.isList
        }

        fun isPair(o: Any?): Boolean {
            return o is List<*> && !o.isEmpty()
        }

        fun isNull(obj: Any?): Boolean {
            return (obj as? List<*>)?.isEmpty() ?: (obj == null)
        }

        /* Use this method to print all lists */
        fun toString(list: List<*>): String {
            if (list.isEmpty()) {
                return "()"
            }
            /* Cons cell */
            val sb = StringBuilder()
            sb.append("(")
            if (!isList(list)) {
                sb.append(Writer.write(list[0]))
                var cdr = list[list.size - 1]
                while (cdr is Cons<*>) {
                    sb.append(" ").append(Writer.write(cdr.first))
                    cdr = cdr.last
                }
                /* Dotted notation */
                sb.append(" . ").append(Writer.write(cdr))
            } else {
                /* List */
                for (i in 0..list.size - 1 - 1) {
                    val e = list[i]
                    sb.append(if (e === list) "(this list)" else Writer.write(e)).append(' ')
                }
                sb.append(Writer.write(list[list.size - 1]))
            }
            return sb.append(')').toString()
        }

        /* Non-recursively flatten a list (or a chain of conses) */
        fun <E> flatten(list: List<E>): List<E> {
            val result = ArrayList<E>()
            val queue = LinkedList(list)
            while (!queue.isEmpty()) {
                val e = queue.remove()
                if (e is List<*>) {
                    queue.addAll(e as List<E>)
                } else {
                    result.add(e)
                }
            }
            return result
        }
    }
}
