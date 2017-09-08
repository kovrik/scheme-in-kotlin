package core.exceptions

class ArityException(message: String) : IllegalArgumentException(message, null) {

    constructor(name: String, min: Int, max: Int, given: Int) :
            this((if (name.isEmpty()) "#<procedure>" else name) +
                    ": arity mismatch; the expected number of arguments does not match the given number (" +
                    "expected: " + (if (min == max) min else if (max > 255) "at least " + min else min.toString() +
                    " to " + max) + ", given: " + given + ")")

    @Synchronized override fun fillInStackTrace() = null
}
