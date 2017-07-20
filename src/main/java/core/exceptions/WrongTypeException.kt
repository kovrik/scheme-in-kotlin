package core.exceptions

import core.procedures.AFn
import core.procedures.IFn
import core.scm.BigComplex
import core.scm.IAssoc
import core.scm.IDeref
import core.scm.IPort
import core.scm.MutableString
import core.scm.Type
import core.writer.Writer

class WrongTypeException : IllegalArgumentException {

    constructor(message: String) : super(message)

    constructor(name: String, expected: String, given: Any?) : super((if (name.isEmpty()) "#<procedure>" else name) +
                ": type mismatch; (expected: $expected, given: ${Writer.write(given)})", null)

    constructor(name: String, expected: Class<*>, given: Any?) : this(name, CUSTOM_TYPE_NAMES[expected] ?: expected.simpleName, given)

    @Synchronized override fun fillInStackTrace() = null

    companion object {
        /* Override type names for some classes */
        private val CUSTOM_TYPE_NAMES = hashMapOf(
                Int::class.javaObjectType   to "Integer",
                Long::class.javaObjectType  to "Integer",
                Type.Rational::class.java   to "Rational",
                BigComplex::class.java      to "Complex",
                CharSequence::class.java    to "String",
                StringBuilder::class.java   to "MutableString",
                MutableString::class.java   to "MutableString",
                IFn::class.java             to "Procedure",
                AFn::class.java             to "Procedure",
                Type.ProperList::class.java to "List",
                IPort::class.java           to "Port",
                Map.Entry::class.java       to "MapEntry",
                IAssoc::class.java          to "Associative",
                IDeref::class.java          to "Delay or Promise or Future"
        )
    }
}
