package core.utils

import core.exceptions.IllegalSyntaxException
import core.exceptions.WrongTypeException
import core.procedures.math.Expt
import core.procedures.math.Multiplication
import core.procedures.math.ToExact
import core.procedures.math.ToInexact
import core.reader.Reader
import core.scm.*
import core.writer.Writer
import java.math.BigDecimal
import java.math.BigInteger
import java.math.MathContext
import java.math.RoundingMode

object Utils {

    const val DEFAULT_SCALE = 16
    val ROUNDING_MODE = RoundingMode.HALF_EVEN
    val DEFAULT_CONTEXT = MathContext(DEFAULT_SCALE, ROUNDING_MODE)

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

    private val BIG_DECIMAL_RADICES = HashMap<Int, BigDecimal>().apply {
        (Character.MIN_RADIX..Character.MAX_RADIX).forEach { put(it, BigDecimal(it)) }
    }

    fun getRadixByChar(radixChar: Char?) = NAMED_RADICES[radixChar] ?: 10

    /* Threshold after which we switch to BigDecimals */
    private val RADIX_THRESHOLDS = hashMapOf(2  to 63, 3  to 39, 4  to 31, 5  to 27, 6  to 24, 7  to 22, 8  to 21,
                                             9  to 19, 10 to 18, 11 to 18, 12 to 17, 13 to 17, 14 to 16, 15 to 16,
                                             16 to 15, 17 to 15, 18 to 15, 19 to 14, 20 to 14, 21 to 14, 22 to 14,
                                             23 to 13, 24 to 13, 25 to 13, 26 to 13, 27 to 13, 28 to 13, 29 to 12,
                                             30 to 12, 31 to 12, 32 to 12, 33 to 12, 34 to 12, 35 to 12, 36 to 12)

    private const val alphabet = "23456789abcdefghijklmnopqrstuvwxyz"
    private val RADIX_CHARS = HashMap<Int, String>().apply {
        put(2,  "#+-.01")
        alphabet.forEachIndexed { i, c ->
            put(i + 3, this[i + 2] + c + c.toUpperCase())
        }
    }

    private val toInexact = ToInexact()
    private val toExact   = ToExact()
    private val expt      = Expt()
    private val multiplication = Multiplication()

    /* Check if digit is valid for a number in a specific radix */
    fun isValidForRadix(c: Char, radix: Int) = RADIX_CHARS[radix]!!.contains(c)

    /* Coerce to DECIMAL64 context if one of the numbers has non-zero scale */
    fun getMathContext(first: BigDecimal, second: BigDecimal): MathContext = when {
        first.scale() > 0 || second.scale() > 0 -> MathContext.DECIMAL64
        else -> MathContext.UNLIMITED
    }

    // FIXME Simplify and cleanup!
    /* Check if string represents a valid number and process it */
    fun preProcessNumber(number: String, exactness: Char?, radix: Int): Any? {
        /* First check if it is a special number */
        SPECIAL_NUMBERS[number]?.let { return it }
        /* Check if that is a complex number (ends with `i` or `I`) */
        val last = number.last()
        if ((last == 'i' || last == 'I') && (number.contains('+') || number.contains('-'))) {
            return processComplexNumber(number, exactness, radix)
        }
        /* Read exponent mark if present */
        val exponentRegex      = if (radix == 16) EXPONENT16_REGEX else EXPONENT_REGEX
        val exponentMarksRegex = if (radix == 16) EXPONENT16_MARKS_REGEX else EXPONENT_MARKS_REGEX
        var exp: Long? = null
        var n = number
        if (exponentRegex.matches(number)) {
            val split = number.split(exponentMarksRegex).dropLastWhile { it.isEmpty() }.toTypedArray()
            n = split[0]
            exp = try {
                processNumber(split[1], radix, true, false, null) as? Long ?:
                              throw IllegalSyntaxException("read: bad exponent: $number")
            } catch (ex: NumberFormatException) {
                throw IllegalSyntaxException("read: bad exponent: $number")
            }
        }
        /* Validate all digits */
        var hasSign = 0
        var hasAtLeastOneDigit = false
        var isIntegral = true
        var hasHashChar = false
        var hasSlash = false
        var i = -1
        for (c in n.toCharArray()) {
            i += 1
            when (c) {
                /* Multiple decimal points are not allowed*/
                '.' -> if (isIntegral) isIntegral = false else return Symbol.intern(number)
                '+' -> if (i == 0) hasSign = 1 else return Symbol.intern(number)
                '-' -> if (i == 0) hasSign = 1 else return Symbol.intern(number)
                '#' -> when {
                    hasAtLeastOneDigit -> hasHashChar = true
                    else -> return Symbol.intern(number)
                }
                /* Check if it is a rational number and if it is valid */
                '/' -> when {
                    hasSlash || !isIntegral -> return Symbol.intern(number)
                    else -> hasSlash = true
                }
                else -> when {
                    isValidForRadix(c, radix) -> hasAtLeastOneDigit = true
                    else -> return Symbol.intern(number)
                }
            }
        }
        if (!hasAtLeastOneDigit) {
            return Symbol.intern(number)
        }
        if (hasHashChar) {
            if (HASH_REGEX.matches(n)) {
                n = n.replace('#', '0')
            } else {
                return Symbol.intern(number)
            }
        }
        val exact = when (exactness) {
            null -> exp == null && !hasHashChar && (hasSlash || isIntegral)
            else -> Reader.isExact(exactness)
        }
        /* Rational and Integral numbers are exact by default */
        val threshold = RADIX_THRESHOLDS[radix]!!
        if (hasSlash) {
            val (numerator, denominator) = n.split('/')
            val useBigNum = numerator.length > threshold + hasSign || denominator.length > threshold + hasSign
            return processRationalNumber(numerator, denominator, radix, exact, useBigNum, exp)
        }
        val useBigNum = n.length > threshold + hasSign
        return processNumber(n, radix, exact, useBigNum, exp)
    }

    private fun processComplexNumber(number: String, exactness: Char?, radix: Int): Any? {
        /* Assume that we have a complex number and try to parse it */
        val p = maxOf(number.lastIndexOf('+'), number.lastIndexOf('-'))
        val r = number.substring(0, p)
        var re: Any? = 0L
        if (!r.isEmpty()) {
            re = preProcessNumber(r, exactness, radix)
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
        return if (isZero(re) && isZero(im)) 0L else BigComplex(toBigDecimal(re as Number), toBigDecimal(im as Number))
    }

    /* Parse string into a number */
    private fun processNumber(number: String, r: Int, exact: Boolean, useBigNum: Boolean, exp: Long?): Number? {
        var result: Number
        val dotPos = number.indexOf('.')
        if (useBigNum) {
            if (dotPos < 0) {
                result = BigInteger(number, r)
            } else {
                /* Remove dot */
                val num = number.replace(".", "")
                /* Process radix */
                var bigDecimal = if (r == 10) BigDecimal(num) else BigDecimal(BigInteger(num, r))
                /* Process radix for a number with decimal point */
                bigDecimal = bigDecimal.divide(BIG_DECIMAL_RADICES[r]!!.pow(num.length - dotPos), MathContext.UNLIMITED)
                if (bigDecimal.stripTrailingZeros().scale() == 0) {
                    bigDecimal = bigDecimal.setScale(1, ROUNDING_MODE)
                }
                result = bigDecimal
            }
        } else {
            result = if (dotPos < 0) {
                number.toLong(r)
            } else {
                if (r == 10) {
                    number.toDouble()
                } else {
                    /* Remove dot */
                    val num = number.replace(".", "")
                    num.toLong(r) / Math.pow(r.toDouble(), (num.length - dotPos).toDouble())
                }
            }
        }
        if (exp != null && !isZero(exp)) {
            if (exp > 999999) {
                return if (isPositive(result)) Double.POSITIVE_INFINITY else Double.NEGATIVE_INFINITY
            } else if (exp < -999) {
                return if (isPositive(result)) 0.0 else -0.0
            }
            if (r == 10 && !exact) {
                return (result.toString() + "E" + exp).toDouble()
            } else {
                result = multiplication(result, expt(r.toLong(), exp))
            }
        }
        return processExactness(result, exact)
    }

    private fun processExactness(number: Number?, exact: Boolean): Number? {
        if (!exact) {
            return toInexact(number)
        }
        /* Racket's Reader does not convert into exact numbers 'properly':
         * #e2.3 returns 23/10
         * but (inexact->exact 2.3) returns 2589569785738035/1125899906842624
         * Guile returns 2589569785738035/1125899906842624 in both cases.
         */
        if (isInexact(number)) {
            if (number is Double) {
                val bigDecimal = toBigDecimal(number)
                val scale = bigDecimal.scale()
                return BigRatio.valueOf(bigDecimal.movePointRight(scale).toBigInteger(), BigInteger.TEN.pow(scale))
            }
            return toExact(number)
        }
        return number
    }

    /* Parse string into a rational number */
    private fun processRationalNumber(numerator: String, denominator: String, r: Int, exact: Boolean,
                                      useBigNum: Boolean, exp: Long?): Number? {

        var number: Number? = BigRatio.valueOf(processNumber(numerator, r, true, useBigNum, null).toString(),
                                               processNumber(denominator, r, true, useBigNum, null).toString())
        exp?.let { number = multiplication(number, expt(r, exp)) }
        return when {
            exact -> number
            else  -> toInexact(number)
        }
    }

    fun toBigDecimal(number: Number): BigDecimal = when (number) {
        is BigDecimal -> number
        is Long       -> BigDecimal.valueOf(number)
        is BigInteger -> BigDecimal(number)
        is Double     -> BigDecimal.valueOf(number)
        is BigRatio   -> number.toBigDecimal()
        is BigComplex -> throw UnsupportedOperationException("undefined for complex!")
        else          -> BigDecimal(number.toString())
    }

    fun toBigInteger(number: Number): BigInteger = when (number) {
        is BigInteger -> number
        is Long       -> BigInteger.valueOf(number)
        is Double     -> BigInteger.valueOf(number.toLong())
        is BigComplex -> throw UnsupportedOperationException("undefined for complex!")
        else          -> BigInteger(number.toString())
    }

    private fun toBigRatio(number: Number) = when (number) {
        is BigRatio   -> number
        is BigInteger -> BigRatio.valueOf(toBigInteger(number), BigInteger.ONE)
        is BigComplex -> throw UnsupportedOperationException("undefined for complex!")
        else          -> BigRatio.valueOf(number.toString(), "1")
    }

    fun isRational(o: Any?) = when (o) {
        !is Number     -> false
        is BigComplex  -> false
        is Double      -> o.isFinite()
        is Float       -> o.isFinite()
        else           -> true
    }

    fun isExact(o: Any?): Boolean = when (o) {
        null          -> false
        is Long, is BigRatio, is Int, is BigInteger, is Short, is Byte -> true
        is BigDecimal -> o.scale() == 0
        is BigComplex -> isExact(o.re) && isExact(o.im)
        else          -> false
    }

    fun isInexact(o: Any?) = !isExact(o)

    fun isInteger(o: Any?) = when (o) {
        null           -> false
        is Long, is Int, is BigInteger, is Short, is Byte -> true
        is BigDecimal  -> o.signum() == 0 || o.scale() <= 0 || o.stripTrailingZeros().scale() <= 0
        is BigRatio    -> o.isDenominatorEqualToOne
        is Double      -> o == Math.floor(o) && o.isFinite()
        else           -> false
    }

    fun isExactInteger(o: Any?) = isExact(o) && isInteger(o)

    fun isZero(o: Any?) = when (o) {
        null          -> false
        is Long       -> o.compareTo(0L) == 0
        is Double     -> Math.signum(o) == 0.0
        is BigRatio   -> o.signum() == 0
        is BigDecimal -> o.signum() == 0
        is Int        -> Integer.signum(o) == 0
        is Short      -> Integer.signum(o.toInt()) == 0
        is Byte       -> Integer.signum(o.toInt()) == 0
        is Float      -> Math.signum(o) == 0f
        is BigInteger -> o.signum() == 0
        else          -> false
    }

    fun isOne(o: Any?) = when (o) {
        null          -> false
        is Long       -> o == 1L
        is Double     -> o == 1
        is BigRatio   -> o.isOne
        is BigDecimal -> o.compareTo(BigDecimal.ONE) == 0
        is Int        -> o == 1
        is Short      -> o.toInt() == 1
        is Byte       -> o.toInt() == 1
        is Float      -> java.lang.Float.floatToRawIntBits(o) == 1
        is BigInteger -> o.compareTo(BigInteger.ONE) == 0
        else          -> false
    }

    fun isPositive(o: Any?) = when (o) {
        null          -> false
        is Long       -> o > 0
        is Double     -> Math.signum(o) == 1.0
        is BigRatio   -> o.signum() == 1
        is BigDecimal -> o.signum() == 1
        is Int        -> Integer.signum(o) == 1
        is Short      -> Integer.signum(o.toInt()) == 1
        is Byte       -> Integer.signum(o.toInt()) == 1
        is Float      -> Math.signum(o) == 1f
        is BigInteger -> o.signum() == 1
        else          -> false
    }

    fun isNegative(o: Any?) = when (o) {
        null          -> false
        is Long       -> o < 0
        is Double     -> Math.signum(o) == -1.0
        is BigRatio   -> o.signum() == -1
        is BigDecimal -> o.signum() == -1
        is Int        -> Integer.signum(o) == -1
        is Short      -> Integer.signum(o.toInt()) == -1
        is Byte       -> Integer.signum(o.toInt()) == -1
        is Float      -> Math.signum(o) == -1f
        is BigInteger -> o.signum() == -1
        else          -> false
    }

    fun isExactNonNegativeInteger(o: Any?) = isExact(o) && isInteger(o) && !isNegative(o)

    fun isReal(o: Any?) = o is Number && o !is BigComplex

    fun isFinite(number: Number?) = when (number) {
        is Double -> number.isFinite()
        is Float  -> number.isFinite()
        else      -> true
    }

    fun isNaN(number: Number?) = number == Double.NaN || number == Float.NaN

    fun isPositiveInfinity(number: Number?) = number == Double.POSITIVE_INFINITY || number == Float.POSITIVE_INFINITY

    fun isNegativeInfinity(number: Number?) = number == Double.NEGATIVE_INFINITY || number == Float.NEGATIVE_INFINITY

    /**
     * Inexactness 'taint'
     * Computations that involve an inexact number produce inexact results,
     * so that inexactness acts as a kind of taint on numbers.
     * See https://docs.racket-lang.org/guide/numbers.html
     */
    fun inexactnessTaint(result: Number, other: Number?) = when {
        isInexact(other) && isExact(result) -> toInexact(result)
        else -> result
    }

    fun downcastNumber(number: Number) = when {
        number is BigRatio && number.isDenominatorEqualToOne -> number.numerator.tryDowncast()
        number is BigDecimal -> number.tryDowncast()
        number is BigInteger -> number.tryDowncast()
        else                 -> number
    }

    /* Try to downcast Big Decimal to a smaller type (if possible) */
    private fun BigDecimal.tryDowncast(): Number {
        /* Same checks are performed in longValueExact() method,
         * but we don't want exception to be thrown, just return the number */
        if (!isInteger(this)) {
            return this
        }
        return try {
            this.longValueExact()
        } catch (e: ArithmeticException) {
            /* Down-casting has failed, ignore and cast to BigInteger then */
            this.toBigInteger()
        }
    }

    /* Try to downcast Big Integer to a smaller type (if possible) */
    private fun BigInteger.tryDowncast(): Number {
        /* Same checks are performed in longValueExact() method,
         * but we don't want exception to be thrown, just return the this */
        if (this.bitLength() <= 63) {
            return this.toLong()
        }
        if (isInteger(this)) {
            try {
                return this.longValueExact()
            } catch (e: ArithmeticException) {
                /* Down-casting has failed, ignore and return the original this */
            }
        }
        return this
    }

    fun isBitOpSupported(obj: Any) = if (obj is Byte || obj is Short || obj is Int || obj is Long) {
        true
    } else {
        throw WrongTypeException("bit operation not supported for: ${Writer.write(obj)}")
    }

    fun isByte(o: Any?) = (o is Number && isReal(o) && o.toByte().toInt() == o.toInt())
    fun isChar(o: Any?) = (o is Number && isReal(o) && o.toChar().toInt() == o.toInt())

    /**
     * Converts any Object to boolean.
     * Returns FALSE only if value is FALSE itself or null.
     * Returns TRUE otherwise.
     */
    fun toBoolean(value: Any?) = value as? Boolean ?: (value != null)

    fun isSeqable(obj: Any?) = obj == null || obj is Sequence<*> || obj is Iterable<*> || obj is CharSequence ||
                               obj is Map<*, *> || obj is Map.Entry<*, *> || obj is ByteArray || obj is ShortArray ||
                               obj is IntArray  || obj is LongArray || obj is DoubleArray || obj is FloatArray ||
                               obj is BooleanArray || obj is CharArray || obj is Array<*>

    fun isAssoc(obj: Any?) = obj == null || obj is Map<*, *> || obj is Map.Entry<*, *> || obj is IAssoc

    fun toSequence(obj: Any?): Sequence<*> = when (obj) {
        null               -> emptyList<Any?>().asSequence()
        is Sequence<*>     -> obj
        is Iterable<*>     -> obj.asSequence()
        is CharSequence    -> obj.asSequence()
        is Map<*, *>       -> mapIterator(obj).asSequence()
        is Map.Entry<*, *> -> MapEntry(obj).asSequence()
        is ByteArray       -> obj.asSequence()
        is ShortArray      -> obj.asSequence()
        is IntArray        -> obj.asSequence()
        is LongArray       -> obj.asSequence()
        is DoubleArray     -> obj.asSequence()
        is FloatArray      -> obj.asSequence()
        is BooleanArray    -> obj.asSequence()
        is CharArray       -> obj.asSequence()
        is Array<*>        -> obj.asSequence()
        else               -> throw IllegalArgumentException("don't know how to create Sequence from ${obj.javaClass}")
    }

    fun isEmpty(o: Any?) = when (o) {
        null             -> true
        is Sequence<*>   -> !o.iterator().hasNext()
        is Collection<*> ->  o.isEmpty()
        is CharSequence  ->  o.isEmpty()
        is Map<*, *>     ->  o.isEmpty()
        else             ->  false
    }

    fun toAssoc(obj: Any?): IAssoc = when (obj) {
        null                -> Hashmap()
        is IAssoc           -> obj
        is Map.Entry<*, *>  -> MapEntry(obj)
        else                -> throw IllegalArgumentException("don't know how to create Map from ${obj.javaClass}")
    }

    private fun mapIterator(map: Map<*, *>) = object : Iterator<MapEntry> {
        private  val iterator = map.iterator()
        override fun hasNext() = iterator.hasNext()
        override fun next() = MapEntry(iterator.next())
    }

    /**
     * Up-cast two numbers to the same type
     */
    fun upcast(f: Number, s: Number): Pair<Number, Number> = when {
        f.javaClass == s.javaClass   -> Pair(f, s)
        !isFinite(f) || !isFinite(s) -> Pair(f, s)
        isInexact(f) || isInexact(s) -> when {
            f is BigComplex || s is BigComplex -> Pair(BigComplex.of(f), BigComplex.of(s))
            f is BigRatio   || s is BigRatio   -> Pair(f.toDouble(),     s.toDouble())
            f is BigDecimal || s is BigDecimal -> Pair(toBigDecimal(f),  toBigDecimal(s))
            f is BigInteger || s is BigInteger -> Pair(toBigDecimal(f),  toBigDecimal(s))
            f is Double     || s is Double     -> Pair(f.toDouble(),     s.toDouble())
            f is Float      || s is Float      -> Pair(f.toFloat(),      s.toFloat())
            else                               -> Pair(f, s)
        }
        else -> when {
            f is BigComplex || s is BigComplex -> Pair(BigComplex.of(f), BigComplex.of(s))
            f is BigRatio   || s is BigRatio   -> Pair(toBigRatio(f),    toBigRatio(s))
            f is BigDecimal || s is BigDecimal -> Pair(toBigDecimal(f),  toBigDecimal(s))
            f is BigInteger || s is BigInteger -> Pair(toBigInteger(f),  toBigInteger(s))
            f is Long       || s is Long       -> Pair(f.toLong(),       s.toLong())
            f is Int        || s is Int        -> Pair(f.toInt(),        s.toInt())
            f is Short      || s is Short      -> Pair(f.toShort(),      s.toShort())
            f is Byte       || s is Byte       -> Pair(f.toByte(),       s.toByte())
            else                               -> Pair(f, s)
        }
    }
}
