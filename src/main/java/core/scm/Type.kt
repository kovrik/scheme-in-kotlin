package core.scm

import core.exceptions.WrongTypeException
import core.procedures.predicates.Predicate
import core.utils.Utils

object Type {

    /* Marker objects */
    object ProperList
    object Pair
    object ExactNonNegativeInteger
    object Rational
    object Real
    object BitOp
    object BitOpOrBigInt
    object Seqable

    private val TYPE_PREDICATES = hashMapOf(
            String::class.java                  to { o: Any? -> o is CharSequence },
            MutableString::class.java           to { o: Any? -> o is StringBuilder || o is MutableString },
            ProperList::class.java              to Predicate.Companion::isProperList,
            Pair::class.java                    to Predicate.Companion::isPair,
            Rational::class.java                to Utils::isRational,
            Long::class.java                    to Utils::isInteger,
            Long::class.javaObjectType          to Utils::isInteger,
            Int::class.java                     to Utils::isInteger,
            Int::class.javaObjectType           to Utils::isInteger,
            ExactNonNegativeInteger::class.java to Utils::isExactNonNegativeInteger,
            Real::class.java                    to Utils::isReal,
            BitOp::class.java                   to Utils::isBitOpSupported,
            BitOpOrBigInt::class.java           to Utils::isBitOpSupportedOrBigInt,
            IAssoc::class.java                  to Utils::isAssoc,
            Byte::class.java                    to Utils::isByte,
            Byte::class.javaObjectType          to Utils::isByte,
            Seqable::class.java                 to Utils::isSeqable
    )

    fun assertType(name: String, o: Any?, expected: Class<*>) = when {
        o == null -> true
        expected == o.javaClass -> true
        expected.isAssignableFrom(o.javaClass) -> true
        TYPE_PREDICATES.getOrDefault(expected) { false }(o) -> true
        else -> throw WrongTypeException(name, expected, o)
    }
}
