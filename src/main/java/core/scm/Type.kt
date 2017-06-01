package core.scm

import core.procedures.AFn
import core.procedures.IFn
import core.utils.Utils

import java.util.function.Predicate

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
    object ExactPositiveInteger
    object Real
    object BitOp

    /* Override type names for some classes */
    private val TYPE_NAME_MAPPINGS = hashMapOf(
            Long::class.javaObjectType to "Integer",
            BigRatio::class.java       to "Rational",
            BigComplex::class.java     to "Complex",
            CharSequence::class.java   to "String",
            StringBuilder::class.java  to "MutableString",
            MutableString::class.java  to "MutableString",
            IFn::class.java            to "Procedure",
            AFn::class.java            to "Procedure",
            ProperList::class.java     to "List",
            IPort::class.java          to "Port",
            Map.Entry::class.java      to "MapEntry",
            IAssoc::class.java         to "Associative"
    )

    private val TYPE_PREDICATES = hashMapOf(
            String::class.java                  to Predicate<Any> { o -> o is CharSequence },
            MutableString::class.java           to Predicate<Any> { o -> o is StringBuilder || o is MutableString },
            ProperList::class.java              to Predicate<Any> { Cons.isProperList(it) },
            Pair::class.java                    to Predicate<Any> { Cons.isPair(it) },
            BigRatio::class.java                to Predicate<Any> { Utils.isRational(it) },
            Long::class.java                    to Predicate<Any> { Utils.isInteger(it) },
            Long::class.javaObjectType          to Predicate<Any> { Utils.isInteger(it) },
            Int::class.java                     to Predicate<Any> { Utils.isInteger(it) },
            Int::class.javaObjectType           to Predicate<Any> { Utils.isInteger(it) },
            ExactPositiveInteger::class.java    to Predicate<Any> { Utils.isExactPositiveInteger(it) },
            ExactNonNegativeInteger::class.java to Predicate<Any> { Utils.isExactNonNegativeInteger(it) },
            Real::class.java                    to Predicate<Any> { Utils.isReal(it) },
            BitOp::class.java                   to Predicate<Any> { Utils.isBitOpSupported(it) },
            IAssoc::class.java                  to Predicate<Any> { Utils.isAssoc(it) }
    )

    fun nameOf(clazz: Class<*>): String = TYPE_NAME_MAPPINGS.getOrDefault(clazz, clazz.simpleName)

    @JvmStatic fun checkType(o: Any?, expected: Class<*>): Boolean {
        /* Nil is possible value for any data type */
        if (o == null) return true
        if (expected == o.javaClass || expected.isAssignableFrom(o.javaClass)) return true
        return TYPE_PREDICATES[expected]?.test(o) ?: false
    }
}
