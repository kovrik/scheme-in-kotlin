package core.scm

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.IFn
import core.utils.Utils

object Type {

    /* Marker classes for FnArgs annotation
     *
     * FnArgs can't cover all numerical types because of the following limitation:
     *
     * The return type of a method declared in an annotation type must be one of the following,
     * or a compile-time error occurs:
     * - Primitive type
     * - String
     * - Class or an invocation of Class (§4.5)
     * - Enum type
     * - Annotation type
     * - Array type whose component type is one of the preceding types
     * (see https://docs.oracle.com/javase/specs/jls/se8/html/jls-9.html#jls-9.6.1)
     */
    // TODO Get rid of them
    /* Marker classes for Proper and Improper lists */
    object ProperList
    object Pair
    /* Marker classes for numbers */
    object ExactNonNegativeInteger
    object Rational
    object Real
    object BitOp

    /* Override type names for some classes */
    private val TYPE_NAME_MAPPINGS = hashMapOf(
            Int::class.javaObjectType  to "Integer",
            Long::class.javaObjectType to "Integer",
            Rational::class.java       to "Rational",
            BigComplex::class.java     to "Complex",
            CharSequence::class.java   to "String",
            StringBuilder::class.java  to "MutableString",
            MutableString::class.java  to "MutableString",
            IFn::class.java            to "Procedure",
            AFn::class.java            to "Procedure",
            ProperList::class.java     to "List",
            IPort::class.java          to "Port",
            Map.Entry::class.java      to "MapEntry",
            IAssoc::class.java         to "Associative",
            IDeref::class.java         to "Delay or Promise or Future"
    )

    private val TYPE_PREDICATES = hashMapOf(
            String::class.java                  to { o: Any? -> o is CharSequence },
            MutableString::class.java           to { o: Any? -> o is StringBuilder || o is MutableString },
            ProperList::class.java              to Cons.Companion::isProperList,
            Pair::class.java                    to Cons.Companion::isPair,
            Rational::class.java                to Utils::isRational,
            Long::class.java                    to Utils::isInteger,
            Long::class.javaObjectType          to Utils::isInteger,
            Int::class.java                     to Utils::isInteger,
            Int::class.javaObjectType           to Utils::isInteger,
            ExactNonNegativeInteger::class.java to Utils::isExactNonNegativeInteger,
            Real::class.java                    to Utils::isReal,
            BitOp::class.java                   to Utils::isBitOpSupported,
            IAssoc::class.java                  to Utils::isAssoc
    )

    fun nameOf(clazz: Class<*>): String = TYPE_NAME_MAPPINGS.getOrDefault(clazz, clazz.simpleName)

    @JvmStatic fun assertType(name: String, o: Any?, expected: Class<*>) = when {
        o == null -> true
        expected == o.javaClass -> true
        expected.isAssignableFrom(o.javaClass) -> true
        !TYPE_PREDICATES.getOrDefault(expected, { false })(o) -> throw WrongTypeException(name, expected, o)
        else -> true
    }
}
