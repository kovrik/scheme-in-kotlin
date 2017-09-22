package core.scm

import core.exceptions.WrongTypeException
import core.writer.Writer

// TODO Persistent Data Structures
class Cons<E> : ArrayList<E?> {

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
        else      -> first()
    }

    fun cdr() = if (isProperList) subList(1, size) else (last() as Any)

    override fun toString() = toString(this)

    override fun hashCode() = 31 * super.hashCode() + if (isProperList) 1 else 0

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null) return false
        if (other !is List<*>) return false
        /* Two empty lists are always equal */
        if (this.size == 0 && other.size == 0) return true
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
        val EMPTY = Cons<Nothing>()

        fun <E> cons(car: E?, cdr: E?) = Cons(car, cdr ?: EMPTY)

        fun <E> list(): Cons<E?> = Cons()
        fun <E> list(e: E?): Cons<E?> = list<E?>().apply { add(e) }
        fun <E> list(e1: E?, e2: E?): Cons<E?> = list<E?>(e1).apply { add(e2) }
        fun <E> list(e1: E?, e2: E?, e3: E?): Cons<E?> = list<E?>(e1, e2).apply { add(e3) }
        fun <E> list(e1: E?, e2: E?, e3: E?, e4: E?): Cons<E?> = list<E?>(e1, e2, e3).apply { add(e4) }
        fun <E> list(e1: E?, e2: E?, e3: E?, e4: E?, e5: E?): Cons<E?> = list<E?>(e1, e2, e3, e4).apply { add(e5) }

        fun <E> list(elements: Array<E?>) = if (elements.isEmpty()) EMPTY else list(elements.asList())

        fun <E> list(c: Collection<E?>) = if (c.isEmpty()) EMPTY else Cons(c)

        /* Return true if o is a Proper List */
        fun isProperList(o: Any?) = o is List<*> && o !is Cons<*> || o is Cons<*> && o.isProperList || o is Sequence<*>

        fun isPair(o: Any?) = o is Pair<*, *> || o is List<*> && !o.isEmpty()

        /* Use this method to print all lists */
        fun toString(list: List<*>) = when {
            list.isEmpty() -> "()"
            isProperList(list) -> StringBuilder("(").apply {
                for (i in 0..list.size - 2) {
                    append(if (list[i] === list) "(this list)" else Writer.write(list[i])).append(' ')
                }
                append(if (list.last() === list) "(this list)" else Writer.write(list.last())).append(')')
            }.toString()
            else -> StringBuilder("(").apply {
                append(Writer.write(list.first()))
                var cdr = list.last()
                while (cdr is Pair<*, *> || cdr is Cons<*>) {
                    when (cdr) {
                        is Cons<*> -> {
                            append(' ').append(Writer.write(cdr.first()))
                            cdr = cdr.last()
                        }
                        is Pair<*, *> -> {
                            append(' ').append(Writer.write(cdr.first))
                            cdr = cdr.second
                        }
                    }
                }
                /* Dotted notation */
                append(" . ").append(Writer.write(cdr)).append(')')
            }.toString()
        }
    }
}
