package core.scm

import core.procedures.predicates.Predicate

@Deprecated(message = "Use Kotlin/Java collections and/or Pair/MutablePair")
class Cons<E> private constructor(car: E?, cdr: E?) : ArrayList<E?>() {

    companion object {
        fun <E> cons(car: E?, cdr: E?) = Cons(car, cdr ?: emptyList<Nothing>())
    }

    /* Is it a Proper List or an Improper one?
     * Proper lists just have this flag set to true, they don't end with empty list.
     **/
    val isProperList: Boolean

    init {
        add(car)
        isProperList = Predicate.isProperList(cdr)
        when (isProperList) {
            true -> when (cdr) {
                is Sequence<*> -> addAll(cdr as Sequence<E>)
                else -> addAll(cdr as List<E>)
            }
            false -> add(cdr)
        }
    }

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
}
