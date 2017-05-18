package core.exceptions

import core.scm.Type
import core.writer.Writer

class WrongTypeException : IllegalArgumentException {

    constructor(message: String) : super(message)

    constructor(name: String, expected: String, given: Any) : super((if (name.isEmpty()) "#<procedure>" else name) + ": type mismatch; " + "(" +
            "expected: " + expected + ", given: " + Writer.write(given) + ")", null)

    constructor(name: String, expected: Class<*>, given: Any) : this(name, Type.nameOf(expected), given)

    @Synchronized override fun fillInStackTrace(): Throwable? {
        return null
    }
}
