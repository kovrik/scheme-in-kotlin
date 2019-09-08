package core.utils

import core.exceptions.IllegalSyntaxException
import core.exceptions.WrongTypeException
import core.procedures.math.Expt
import core.procedures.math.Multiplication
import core.procedures.math.ToExact
import core.procedures.math.ToInexact
import core.scm.*
import core.Writer
import java.math.BigDecimal
import java.math.BigInteger
import java.math.MathContext
import java.math.RoundingMode
import kotlin.math.floor
import kotlin.math.pow
import kotlin.math.sign

object Utils {

    const val DEFAULT_SCALE = 16
    val ROUNDING_MODE = RoundingMode.HALF_EVEN
    val DEFAULT_CONTEXT = MathContext(DEFAULT_SCALE, ROUNDING_MODE)
    val TWO = "2".toBigDecimal()

    private val HASH_REGEX = ".+(#+\\.?+#?)/?(#+\\.?+#?)?$".toRegex()

    private val EXPONENT_MARKS_REGEX   = "[sldefSLDEF]".toRegex()
    private val EXPONENT16_MARKS_REGEX = "[slSL]".toRegex()
    private val EXPONENT_REGEX         = ".+$EXPONENT_MARKS_REGEX[+-]?\\d+(\\.\\d*)?$".toRegex()
    private val EXPONENT16_REGEX       = ".+$EXPONENT16_MARKS_REGEX[+-]?\\w+$".toRegex()

    private val SPECIAL_NUMBERS = hashMapOf("+nan.0" to Double.NaN,
                                            "-nan.0" to Double.NaN,
                                            "+inf.0" to Double.POSITIVE_INFINITY,
                                            "-inf.0" to Double.NEGATIVE_INFINITY)

    private val NAMED_RADICES = hashMapOf('b' to 2,  'B' to 2,
                                          'o' to 8,  'O' to 8,
                                          'd' to 10, 'D' to 10,
                                          'x' to 16, 'X' to 16)

    private val BIG_DECIMAL_RADICES = (Character.MIN_RADIX..Character.MAX_RADIX).associateBy({ it }, { it.toBigDecimal() })

    fun getRadixByChar(radixChar: Char?) = NAMED_RADICES[radixChar] ?: 10

    private val RADIX_DIGITS = HashMap<Int, String>().apply {
        put(2,  "#+-.01")
        "23456789abcdefghijklmnopqrstuvwxyz".forEachIndexed { i, c ->
            put(i + 3, this[i + 2] + c + c.toUpperCase())
        }
    }

    private val toInexact = ToInexact()
    private val toExact   = ToExact()
    private val expt      = Expt()
    private val multiplication = Multiplication()

    /* Check if digit is valid for a number in a specific radix */
    fun isValidForRadix(c: Char, radix: Int) = c in RADIX_DIGITS[radix]!!

    /* Coerce to DECIMAL64 context if one of the numbers has non-zero scale */
    fun getMathContext(first: BigDecimal, second: BigDecimal): MathContext = when {
        first.scale() > 0 || second.scale() > 0 -> MathContext.DECIMAL64
        else -> MathContext.UNLIMITED
    }

    // FIXME Simplify and cleanup!
    /* Check if string represents a valid number and process it */
    fun preProcessNumber(number: String, exactness: Boolean?, radix: Int): Any? {
        /* First check if it is a special number */
        SPECIAL_NUMBERS[number]?.let { return it }
        /* Check if that is a complex number (ends with `i` or `I`) */
        if (number.endsWith('i', ignoreCase = true) && ('+' in number || '-' in number)) {
            return processComplexNumber(number, exactness, radix)
        }
        /* Read exponent mark if present */
        val exponentRegex      = if (radix == 16) EXPONENT16_REGEX else EXPONENT_REGEX
        val exponentMarksRegex = if (radix == 16) EXPONENT16_MARKS_REGEX else EXPONENT_MARKS_REGEX
        var exp: Long? = null
        var n = number
        if (number matches exponentRegex) {
            val split = number.split(exponentMarksRegex).dropLastWhile(String::isEmpty)
            n = split[0]
            exp = try {
                processNumber(split[1], radix, true, null) as? Long ?:
                              throw IllegalSyntaxException("read: bad exponent: $number")
            } catch (ex: NumberFormatException) {
                throw IllegalSyntaxException("read: bad exponent: $number")
            }
        }
        /* Validate all digits */
        var hasAtLeastOneDigit = false
        var isIntegral = true
        var hasHashChar = false
        var hasSlash = false
        for ((index, c) in n.withIndex()) {
            when (c) {
                /* Multiple decimal points are not allowed*/
                '.'  -> if (isIntegral) isIntegral = false else return Symbol.intern(number)
                '+'  -> if (index != 0) return Symbol.intern(number)
                '-'  -> if (index != 0) return Symbol.intern(number)
                '#'  -> hasHashChar = true
                /* Check if it is a rational number and if it is valid */
                '/'  -> if (!hasSlash && isIntegral) hasSlash = true else return Symbol.intern(number)
                else -> if (isValidForRadix(c, radix)) hasAtLeastOneDigit = true else return Symbol.intern(number)
            }
        }
        if (!hasAtLeastOneDigit) {
            return Symbol.intern(number)
        }
        if (hasHashChar) {
            if (n matches HASH_REGEX) {
                n = n.replace('#', '0')
            } else {
                return Symbol.intern(number)
            }
        }
        val exact = when (exactness) {
            null -> exp == null && !hasHashChar && (hasSlash || isIntegral)
            else -> exactness
        }
        /* Rational and Integral numbers are exact by default */
        if (hasSlash) {
            val (numerator, denominator) = n.split('/')
            return processRationalNumber(numerator, denominator, radix, exact, exp)
        }
        return processNumber(n, radix, exact, exp)
    }

    private fun processComplexNumber(number: String, exactness: Boolean?, radix: Int): Any? {
        /* Assume that we have a complex number and try to parse it */
        val p = maxOf(number.lastIndexOf('+'), number.lastIndexOf('-'))
        val r = number.take(p)
        val re = when (!r.isEmpty()) {
            true  -> preProcessNumber(r, exactness, radix)
            false -> 0L
        }
        if (!isReal(re)) {
            return Symbol.intern(number)
        }

        var i = number.substring(p, number.length - 1)
        if (i.length == 1 && (i[0] == '+' || i[0] == '-')) {
            i += "1"
        }
        val im = preProcessNumber(i, exactness, radix)
        if (!isReal(im)) {
            return Symbol.intern(number)
        }
        return if (isZero(re) && isZero(im)) 0L else Complex(toBigDecimal(re as Number), toBigDecimal(im as Number))
    }

    // TODO Check & optimize
    /* Parse string into a number */
    private fun processNumber(number: String, r: Int, exact: Boolean, exp: Long?): Number? {
        val result: Number
        val dotPos = number.indexOf('.')
        if (dotPos < 0) {
            result = try {
                number.toLong(r)
            } catch (e: NumberFormatException) {
                number.toBigInteger(r)
            }
        } else {
            val sign = if (number[0].isDigit()) 0 else 1
            if (dotPos - sign <= 15) {
                result = when (r) {
                    10   -> number.toDouble()
                    else -> number.removeRange(dotPos, dotPos + 1).let {
                        it.toLong(r) / r.toDouble().pow(it.length - dotPos)
                    }
                }
            } else {
                /* Remove dot */
                val num = number.removeRange(dotPos, dotPos + 1)
                /* Process radix */
                val bigDecimal = when (r) {
                    10   -> num.toBigDecimal().movePointLeft(num.length - dotPos)
                    else -> num.toBigInteger(r).toBigDecimal().divide(BIG_DECIMAL_RADICES[r]!!.pow(num.length - dotPos), MathContext.UNLIMITED)
                }
                /* Process radix for a number with decimal point */
                result = when (bigDecimal.stripTrailingZeros().scale() == 0) {
                    true  -> bigDecimal.setScale(1, ROUNDING_MODE)
                    false -> bigDecimal
                }
            }
        }
        return when {
            exp == null -> processExactness(result, exact)
            exp > 999999 -> if (isPositive(result)) Double.POSITIVE_INFINITY else Double.NEGATIVE_INFINITY
            exp < -999 -> if (isPositive(result)) 0.0 else -0.0
            r == 10 && !exact -> "${result}E$exp".toDouble()
            else -> processExactness(multiplication(result, expt(r.toLong(), exp)), exact)
        }
    }

    private fun processExactness(number: Number?, exact: Boolean) = when {
        !exact -> toInexact(number)
        /* Racket's Reader does not convert into exact numbers 'properly':
         * #e2.3 returns 23/10
         * but (inexact->exact 2.3) returns 2589569785738035/1125899906842624
         * Guile returns 2589569785738035/1125899906842624 in both cases.
         */
        isInexact(number) -> {
            when (number) {
                is Double -> {
                    val bigDecimal = number.toBigDecimal()
                    val scale = bigDecimal.scale()
                    Ratio.valueOf(bigDecimal.movePointRight(scale).toBigInteger(), BigInteger.TEN.pow(scale))
                }
                else -> toExact(number)
            }
        }
        else -> number
    }

    /* Parse string into a rational number */
    private fun processRationalNumber(numerator: String, denominator: String, r: Int, exact: Boolean, exp: Long?): Number? {

        var number: Number? = Ratio.valueOf(processNumber(numerator, r, true, null).toString(),
                                            processNumber(denominator, r, true, null).toString())
        exp?.let { number = multiplication(number, expt(r, exp)) }
        return when {
            exact -> number
            else  -> toInexact(number)
        }
    }

    fun toBigDecimal(number: Any?): BigDecimal = when (number) {
        is BigDecimal -> number
        is Long       -> number.toBigDecimal()
        is BigInteger -> number.toBigDecimal()
        is Double     -> number.toBigDecimal()
        is Ratio      -> number.toBigDecimal()
        is Complex    -> throw UnsupportedOperationException("undefined for complex!")
        else          -> number.toString().toBigDecimal()
    }

    fun toBigInteger(number: Any?): BigInteger = when (number) {
        is BigInteger -> number
        is Long       -> number.toBigInteger()
        is Double     -> number.toLong().toBigInteger()
        is Complex    -> throw UnsupportedOperationException("undefined for complex!")
        else          -> number.toString().toBigInteger()
    }

    private fun toBigRatio(number: Number) = when (number) {
        is Ratio      -> number
        is BigInteger -> Ratio.valueOf(toBigInteger(number), BigInteger.ONE)
        is Complex -> throw UnsupportedOperationException("undefined for complex!")
        else          -> Ratio.valueOf(number.toString(), "1")
    }

    fun isRational(o: Any?) = when (o) {
        !is Number     -> false
        is Complex     -> false
        is Double      -> o.isFinite()
        is Float       -> o.isFinite()
        else           -> true
    }

    fun isExact(o: Any?): Boolean = when (o) {
        null          -> false
        is Long, is Ratio, is Int, is BigInteger, is Short, is Byte -> true
        is BigDecimal -> o.scale() == 0
        is Complex    -> isExact(o.re) && isExact(o.im)
        else          -> false
    }

    fun isInexact(o: Any?) = !isExact(o)

    fun isInteger(o: Any?) = when (o) {
        null           -> false
        is Long, is Int, is BigInteger, is Short, is Byte -> true
        is BigDecimal  -> o.signum() == 0 || o.scale() <= 0 || o.stripTrailingZeros().scale() <= 0
        is Ratio       -> o.isDenominatorEqualToOne
        is Double      -> o == floor(o) && o.isFinite()
        is Float       -> o == floor(o) && o.isFinite()
        else           -> false
    }

    fun isExactInteger(o: Any?) = isExact(o) && isInteger(o)

    fun isZero(o: Any?) = when (o) {
        null          -> false
        is Long       -> o.sign == 0
        is Double     -> o.sign == 0.0
        is Ratio      -> o.signum() == 0
        is BigDecimal -> o.signum() == 0
        is Int        -> o.sign == 0
        is Short      -> o.toInt().sign == 0
        is Byte       -> o.toInt().sign == 0
        is Float      -> o.sign == 0f
        is BigInteger -> o.signum() == 0
        else          -> false
    }

    fun isOne(o: Any?) = when (o) {
        null          -> false
        is Long       -> o == 1L
        is Double     -> o == 1
        is Ratio      -> o.isOne
        is BigDecimal -> o.compareTo(BigDecimal.ONE) == 0
        is Int        -> o == 1
        is Short      -> o.toInt() == 1
        is Byte       -> o.toInt() == 1
        is Float      -> o.toRawBits() == 1
        is BigInteger -> o.compareTo(BigInteger.ONE) == 0
        else          -> false
    }

    fun isPositive(o: Any?) = when (o) {
        null          -> false
        is Long       -> o.sign == 1
        is Double     -> o.sign == 1.0
        is Ratio      -> o.signum() == 1
        is BigDecimal -> o.signum() == 1
        is Int        -> o.sign == 1
        is Short      -> o.toInt().sign == 1
        is Byte       -> o.toInt().sign == 1
        is Float      -> o.sign == 1f
        is BigInteger -> o.signum() == 1
        else          -> false
    }

    fun isNegative(o: Any?) = when (o) {
        null          -> false
        is Long       -> o.sign == -1
        is Double     -> o.sign == -1.0
        is Ratio      -> o.signum() == -1
        is BigDecimal -> o.signum() == -1
        is Int        -> o.sign == -1
        is Short      -> o.toInt().sign == -1
        is Byte       -> o.toInt().sign == -1
        is Float      -> o.sign == -1f
        is BigInteger -> o.signum() == -1
        else          -> false
    }

    fun isExactNonNegativeInteger(o: Any?) = isExact(o) && isInteger(o) && !isNegative(o)

    fun isReal(o: Any?) = o is Number && o !is Complex

    fun isFinite(number: Number?) = when (number) {
        is Double -> number.isFinite()
        is Float  -> number.isFinite()
        else      -> true
    }

    fun isNaN(number: Number?) = number == Double.NaN || number == Float.NaN

    fun isPositiveInfinity(number: Number?) = number == Double.POSITIVE_INFINITY || number == Float.POSITIVE_INFINITY

    fun isNegativeInfinity(number: Number?) = number == Double.NEGATIVE_INFINITY || number == Float.NEGATIVE_INFINITY

    /* Return number of digits of a given BigDecimal number */
    fun integerDigits(n: BigDecimal) = if (n.signum() == 0) 1 else n.precision() - n.scale()

    /**
     * Inexactness 'taint'
     * Computations that involve an inexact number produce inexact results,
     * so that inexactness acts as a kind of taint on numbers.
     * See https://docs.racket-lang.org/guide/numbers.html
     */
    infix fun Number.taint(other: Number) = when {
        isInexact(this) && isExact(other) -> toInexact(other)
        else -> other
    }

    fun downcastNumber(number: Number) = when {
        number is Ratio && number.isDenominatorEqualToOne -> number.numerator.tryDowncast()
        number is BigDecimal -> number.tryDowncast()
        number is BigInteger -> number.tryDowncast()
        else                 -> number
    }

    /* Try to downcast Big Decimal to a smaller type (if possible) */
    /* Same checks are performed in longValueExact() method,
     * but we don't want exception to be thrown, just return the number */
    private fun BigDecimal.tryDowncast() = when {
        isInteger(this) -> try {
            longValueExact()
        } catch (e: ArithmeticException) {
            /* Down-casting has failed, ignore and cast to BigInteger then */
            toBigInteger() as Number
        }
        else -> this
    }

//    fun BigDecimal.sqrt(scale: Int): BigDecimal {
//        var x0 = BigDecimal.ZERO
//        var x1 = BigDecimal(Math.sqrt(this.toDouble()))
//        while (x0 != x1) {
//            x0 = x1
//            x1 = this.divide(x0, scale, RoundingMode.HALF_UP)
//            x1 = x1.add(x0)
//            x1 = x1.divide(Utils.TWO, scale, RoundingMode.HALF_UP)
//        }
//        return x1
//    }

    /* Try to downcast Big Integer to a smaller type (if possible) */
    /* Same checks are performed in longValueExact() method,
     * but we don't want exception to be thrown, just return the this */
    private fun BigInteger.tryDowncast() = try {
        longValueExact()
    } catch (e: ArithmeticException) {
        /* Down-casting has failed, ignore and return the original this */
        this as Number
    }

    fun isBitOpSupported(obj: Any) = when (obj) {
        is Byte, is Short, is Int, is Long -> true
        else -> throw WrongTypeException("bit operation not supported for: ${Writer.write(obj)}")
    }

    fun isBitOpSupportedOrBigInt(obj: Any) = obj is BigInteger || isBitOpSupported(obj)

    fun isByte(o: Any?) = (o is Number && isReal(o) && o.toByte().toInt() == o.toInt())
    fun isChar(o: Any?) = (o is Number && isReal(o) && o.toChar().toInt() == o.toInt())

    /**
     * Converts any Object to boolean.
     * Returns FALSE only if value is FALSE itself or null.
     * Returns TRUE otherwise.
     */
    fun toBoolean(value: Any?) = value as? Boolean ?: (value != null)

    fun isSeqable(obj: Any?) = obj === Unit ||  obj == null || obj is Sequence<*> || obj is Iterable<*> || obj is CharSequence ||
                               obj is Pair<*, *> || obj is MutablePair<*, *> || obj is Map<*, *> || obj is Map.Entry<*, *> ||
                               obj.javaClass.isArray

    fun isAssoc(obj: Any?) = obj == null || obj is Map<*, *> || obj is Map.Entry<*, *> || obj is IAssoc<*, *>

    fun toSequence(obj: Any?): Sequence<*> = when (obj) {
        Unit                 -> emptySequence<Nothing>()
        null                 -> emptySequence<Nothing>()
        is Sequence<*>       -> obj
        is Iterable<*>       -> obj.asSequence()
        is CharSequence      -> obj.asSequence()
        is Pair<*, *>        -> sequenceOf(obj.first, obj.second)
        is MutablePair<*, *> -> sequenceOf(obj.first, obj.second)
        is Map<*, *>         -> mapIterator(obj).asSequence()
        is Map.Entry<*, *>   -> MapEntry(obj).asSequence()
        is ByteArray         -> obj.asSequence()
        is ShortArray        -> obj.asSequence()
        is IntArray          -> obj.asSequence()
        is LongArray         -> obj.asSequence()
        is DoubleArray       -> obj.asSequence()
        is FloatArray        -> obj.asSequence()
        is BooleanArray      -> obj.asSequence()
        is CharArray         -> obj.asSequence()
        is Array<*>          -> obj.asSequence()
        else                 -> throw IllegalArgumentException("don't know how to create Sequence from ${obj.javaClass}")
    }

    fun isEmpty(o: Any?) = when (o) {
        null             -> true
        is Sequence<*>   -> o.none()
        is Collection<*> -> o.none()
        is CharSequence  -> o.none()
        is Map<*, *>     -> o.none()
        else             -> false
    }

    fun <K, V> toAssoc(obj: Any?): IAssoc<K, V> = when (obj) {
        null                -> Hashmap()
        is IAssoc<*, *>     -> obj as IAssoc<K, V>
        is Map.Entry<*, *>  -> MapEntry(obj) as IAssoc<K, V>
        else                -> throw IllegalArgumentException("don't know how to create Map from ${obj.javaClass}")
    }

    private fun mapIterator(map: Map<*, *>) = object : Iterator<MapEntry<*, *>> {
        private  val iterator = map.iterator()
        override fun hasNext() = iterator.hasNext()
        override fun next() = MapEntry(iterator.next())
    }

    fun cons(elements: List<Any?>): Any? = when (elements.size) {
        0 -> throw IllegalArgumentException("don't know how to create Pair from ${Writer.write(elements)}")
        /* (cons* 1) => 1 ; see SRFI-1 */
        1 -> elements.first()
        /* Convert list into cons */
        else -> {
            var pair = Pair(elements[elements.size - 2], elements.last())
            for (n in elements.size - 3 downTo 0) {
                pair = Pair(elements[n], pair)
            }
            pair
        }
    }

    /**
     * Up-cast two numbers to the same type
     */
    fun upcast(f: Number, s: Number) = when {
        f.javaClass == s.javaClass   -> f to s
        !isFinite(f) || !isFinite(s) -> f to s
        isInexact(f) || isInexact(s) -> when {
            f is Complex    || s is Complex    -> Complex.valueOf(f) to Complex.valueOf(s)
            f is Ratio      || s is Ratio      -> f.toDouble()       to s.toDouble()
            f is BigDecimal || s is BigDecimal -> toBigDecimal(f)    to toBigDecimal(s)
            f is BigInteger || s is BigInteger -> toBigDecimal(f)    to toBigDecimal(s)
            f is Double     || s is Double     -> f.toDouble()       to s.toDouble()
            f is Float      || s is Float      -> f.toFloat( )       to s.toFloat()
            else                               -> f to s
        }
        else -> when {
            f is Complex    || s is Complex    -> Complex.valueOf(f) to Complex.valueOf(s)
            f is Ratio      || s is Ratio      -> toBigRatio(f)      to toBigRatio(s)
            f is BigDecimal || s is BigDecimal -> toBigDecimal(f)    to toBigDecimal(s)
            f is BigInteger || s is BigInteger -> toBigInteger(f)    to toBigInteger(s)
            f is Long       || s is Long       -> f.toLong()         to s.toLong()
            f is Int        || s is Int        -> f.toInt()          to s.toInt()
            f is Short      || s is Short      -> f.toShort()        to s.toShort()
            f is Byte       || s is Byte       -> f.toByte()         to s.toByte()
            else                               -> f to s
        }
    }
}
