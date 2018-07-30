package core.procedures

sealed class Arity {

    abstract fun check(size: Int): Boolean

    data class Exactly(val arity: Int) : Arity() {
        override fun check(size: Int) = arity == size
        override fun toString() = "$arity"
    }

    data class Range(val from: Int, val to: Int) : Arity() {
        override fun check(size: Int) = size in from..to
        override fun toString() = "$from to $to"
    }

    data class AtLeast(val atLeast: Int) : Arity() {
        override fun check(size: Int) = size >= atLeast
        override fun toString() = "at least $atLeast"
    }

    data class OneOf(val arities: List<Arity>) : Arity() {
        override fun check(size: Int) = arities.any { it.check(size) }
        override fun toString() = arities.joinToString(separator = " or ", transform = Arity::toString)
    }
}

