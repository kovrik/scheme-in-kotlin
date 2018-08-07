package core.exceptions

import core.procedures.AFn
import core.procedures.IFn
import core.scm.*
import core.Writer

class WrongTypeException : IllegalArgumentException {

    constructor(message: String) : super(message)

    // TODO create separate FiniteSeq class?
    constructor(name: String, expected: String, given: Any?) : super((if (name.isEmpty()) "#<procedure>" else name) +
            ": type mismatch; (expected: $expected," +
            " given: ${if (given is Sequence<*>) "Sequence" else Writer.write(given)})", null)

    constructor(name: String, expected: Class<*>, given: Any?) : this(name, CUSTOM_TYPE_NAMES[expected] ?: expected.simpleName, given)

    @Synchronized override fun fillInStackTrace() = null

    companion object {
        /* Override type names for some classes */
        private val CUSTOM_TYPE_NAMES = hashMapOf(Int::class.java             to "Integer",
                                                  Int::class.javaObjectType   to "Integer",
                                                  Long::class.java            to "Integer",
                                                  Long::class.javaObjectType  to "Integer",
                                                  Type.Rational::class.java   to "Rational",
                                                  Complex::class.java      to "Complex",
                                                  CharSequence::class.java    to "String",
                                                  StringBuilder::class.java   to "Mutable String",
                                                  MutableString::class.java   to "Mutable String",
                                                  IFn::class.java             to "Procedure",
                                                  AFn::class.java             to "Procedure",
                                                  Type.ProperList::class.java to "List",
                                                  IPort::class.java           to "Port",
                                                  Map.Entry::class.java       to "Map Entry",
                                                  IAssoc::class.java          to "Associative",
                                                  IDeref::class.java          to "Delay or Promise or Future",
                                                  MutableHashmap::class.java  to "Mutable Hashmap"
        )
    }
}
