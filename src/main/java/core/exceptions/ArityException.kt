package core.exceptions

import core.procedures.Arity

class ArityException(message: String) : IllegalArgumentException(message, null) {

    constructor(name: String, expected: Arity, given: Int) : this(name.ifEmpty { "#<procedure>" } +
            ": arity mismatch; the expected number of arguments does not match the given number (expected: $expected, given: $given)")

    @Synchronized override fun fillInStackTrace() = null
}
