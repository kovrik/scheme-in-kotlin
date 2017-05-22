package core.scm

import core.procedures.AFn
import core.procedures.IFn
import core.utils.Utils

import java.util.HashMap
import java.util.function.Predicate

object Type {

    /* Override type names for some classes */
    private val TYPE_NAME_MAPPINGS = HashMap<Class<*>, String>()

    init {
        TYPE_NAME_MAPPINGS.put(Long::class.javaObjectType, "Integer")
        TYPE_NAME_MAPPINGS.put(BigRatio::class.java, "Rational")
        TYPE_NAME_MAPPINGS.put(BigComplex::class.java, "Complex")
        TYPE_NAME_MAPPINGS.put(CharSequence::class.java, "String")
        TYPE_NAME_MAPPINGS.put(StringBuilder::class.java, "MutableString")
        TYPE_NAME_MAPPINGS.put(MutableString::class.java, "MutableString")
        TYPE_NAME_MAPPINGS.put(IFn::class.java, "Procedure")
        TYPE_NAME_MAPPINGS.put(AFn::class.java, "Procedure")
        TYPE_NAME_MAPPINGS.put(ProperList::class.java, "List")
        TYPE_NAME_MAPPINGS.put(IPort::class.java, "Port")
        TYPE_NAME_MAPPINGS.put(Map.Entry::class.java, "MapEntry")
        TYPE_NAME_MAPPINGS.put(IAssoc::class.java, "Associative")
    }

    fun nameOf(clazz: Class<*>): String {
        return (TYPE_NAME_MAPPINGS as Map<Class<*>, String>).getOrDefault(clazz, clazz.simpleName)
    }

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

    private val TYPE_PREDICATES = HashMap<Class<*>, Predicate<Any>>()

    init {
        TYPE_PREDICATES.put(String::class.java, Predicate<Any>{ o -> o is CharSequence })
        TYPE_PREDICATES.put(MutableString::class.java, Predicate<Any>{ o -> StringBuilder::class.java == o.javaClass || MutableString::class.java == o.javaClass })
        TYPE_PREDICATES.put(ProperList::class.java, Predicate<Any> { Cons.isList(it) })
        TYPE_PREDICATES.put(Pair::class.java, Predicate<Any> { Cons.isPair(it) })
        TYPE_PREDICATES.put(BigRatio::class.java, Predicate<Any> { Utils.isRational(it) })
        TYPE_PREDICATES.put(Long::class.java, Predicate<Any> { Utils.isInteger(it) })
        TYPE_PREDICATES.put(Long::class.javaObjectType, Predicate<Any> { Utils.isInteger(it) })
        TYPE_PREDICATES.put(Int::class.java, Predicate<Any> { Utils.isInteger(it) })
        TYPE_PREDICATES.put(Int::class.javaObjectType, Predicate<Any> { Utils.isInteger(it) })
        TYPE_PREDICATES.put(ExactPositiveInteger::class.java, Predicate<Any> { Utils.isExactPositiveInteger(it) })
        TYPE_PREDICATES.put(ExactNonNegativeInteger::class.java, Predicate<Any> { Utils.isExactNonNegativeInteger(it) })
        TYPE_PREDICATES.put(Real::class.java, Predicate<Any> { Utils.isReal(it) })
        TYPE_PREDICATES.put(BitOp::class.java, Predicate<Any> { Utils.isBitOpSupported(it) })
        TYPE_PREDICATES.put(IAssoc::class.java, Predicate<Any> { Utils.isAssoc(it) })
    }

    @JvmStatic fun checkType(o: Any?, expected: Class<*>): Boolean {
        /* Nil is possible value for any data type */
        if (o == null) {
            return true
        }
        val actual = o.javaClass
        if (expected == actual || expected.isAssignableFrom(actual)) {
            return true
        }
        val check = TYPE_PREDICATES[expected]
        return check != null && check.test(o)
    }
}
