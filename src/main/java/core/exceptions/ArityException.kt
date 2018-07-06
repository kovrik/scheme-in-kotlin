package core.exceptions

import core.procedures.Arity

class ArityException(message: String) : IllegalArgumentException(message, null) {

    constructor(name: String, expected: Arity, given: Int) : this((if (name.isEmpty()) "#<procedure>" else name) +
            ": arity mismatch; the expected number of arguments does not match the given number (expected: $expected, given: $given)")

    @Synchronized override fun fillInStackTrace() = null
}
