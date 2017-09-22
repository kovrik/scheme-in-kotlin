package core.scm

import core.procedures.predicates.Predicate
import core.writer.Writer

@Deprecated(message = "Use Kotlin/Java collections and/or MutablePair")
class Cons<E> : ArrayList<E?> {

    /* Is it a Proper List or an Improper one?
     * Proper lists just have this flag set to true, they don't end with empty list.
     **/
    var isProperList = true

    private constructor() : super()
    private constructor(c: Collection<E>) : super(c)
    private constructor(car: E?, cdr: E?) : super() {
        add(car)
        isProperList = Predicate.isProperList(cdr)
        when {
            isProperList -> when (cdr) {
                is Sequence<*> -> addAll(cdr as Sequence<E>)
                else -> addAll(cdr as List<E>)
            }
            else -> add(cdr)
        }
    }

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
        if (other is MutablePair && isProperList) return false
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

        fun <E> cons(car: E?, cdr: E?) = Cons(car, cdr ?: emptyList<Nothing>())

        fun <E> list(c: Collection<E?>) = if (c.isEmpty()) EMPTY else Cons(c)

        /* Use this method to print all lists */
        fun toString(list: List<*>) = when {
            list.isEmpty() -> "()"
            Predicate.isProperList(list) -> StringBuilder("(").apply {
                for (i in 0..list.size - 2) {
                    append(if (list[i] === list) "(this list)" else Writer.write(list[i])).append(' ')
                }
                append(if (list.last() === list) "(this list)" else Writer.write(list.last())).append(')')
            }.toString()
            else -> StringBuilder("(").apply {
                append(Writer.write(list.first()))
                var cdr = list.last()
                while (cdr is MutablePair || cdr is Cons<*>) {
                    when (cdr) {
                        is Cons<*> -> {
                            append(' ').append(Writer.write(cdr.first()))
                            cdr = cdr.last()
                        }
                        is MutablePair -> {
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
