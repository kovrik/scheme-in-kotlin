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
import java.util.regex.Pattern
import java.util.stream.IntStream

object Utils {

    const val DEFAULT_SCALE = 16
    val ROUNDING_MODE = RoundingMode.HALF_EVEN
    val DEFAULT_CONTEXT = MathContext(DEFAULT_SCALE, ROUNDING_MODE)

    private val HASH_PATTERN = Pattern.compile(".+(#+\\.?+#?)/?(#+\\.?+#?)?$")

    private val EXPONENT_MARKS_PATTERN   = "[sldefSLDEF]".toRegex()
    private val EXPONENT16_MARKS_PATTERN = "[slSL]".toRegex()
    private val EXPONENT_PATTERN   = Pattern.compile(".+$EXPONENT_MARKS_PATTERN[+-]?\\d+(\\.\\d*)?$")
    private val EXPONENT16_PATTERN = Pattern.compile(".+$EXPONENT16_MARKS_PATTERN[+-]?\\w+$")

    private val SPECIAL_NUMBERS = hashMapOf("+nan.0" to Double.NaN,
                                            "-nan.0" to Double.NaN,
                                            "+inf.0" to Double.POSITIVE_INFINITY,
                                            "-inf.0" to Double.NEGATIVE_INFINITY)

    private val NAMED_RADICES = hashMapOf('b' to 2,  'B' to 2,
                                          'o' to 8,  'O' to 8,
                                          'd' to 10, 'D' to 10,
                                          'x' to 16, 'X' to 16)

    private val BIG_DECIMAL_RADICES = HashMap<Int, BigDecimal>()
    init {
        IntStream.rangeClosed(2, 16).forEach { r -> BIG_DECIMAL_RADICES.put(r, BigDecimal(r)) }
    }

    fun getRadixByChar(radixChar: Char?): Int {
        return NAMED_RADICES[radixChar] ?: 10
    }

    /* Threshold after which we switch to BigDecimals */
    private val RADIX_THRESHOLDS = hashMapOf(2  to 63, 3  to 39, 4  to 31, 5  to 27, 6  to 24, 7  to 22, 8  to 21,
                                             9  to 19, 10 to 18, 11 to 18, 12 to 17, 13 to 17, 14 to 16, 15 to 16,
                                             16 to 15)

    private val RADIX_CHARS = HashMap<Int, String>()
    init {
        RADIX_CHARS.put(2,  "#+-.01")
        RADIX_CHARS.put(3,  RADIX_CHARS[2] + "2")
        RADIX_CHARS.put(4,  RADIX_CHARS[3] + "3")
        RADIX_CHARS.put(5,  RADIX_CHARS[4] + "4")
        RADIX_CHARS.put(6,  RADIX_CHARS[5] + "5")
        RADIX_CHARS.put(7,  RADIX_CHARS[6] + "6")
        RADIX_CHARS.put(8,  RADIX_CHARS[7] + "7")
        RADIX_CHARS.put(9,  RADIX_CHARS[8] + "8")
        RADIX_CHARS.put(10, RADIX_CHARS[9] + "9")
        RADIX_CHARS.put(11, RADIX_CHARS[10] + "aA")
        RADIX_CHARS.put(12, RADIX_CHARS[11] + "bB")
        RADIX_CHARS.put(13, RADIX_CHARS[12] + "cC")
        RADIX_CHARS.put(14, RADIX_CHARS[13] + "dD")
        RADIX_CHARS.put(15, RADIX_CHARS[14] + "eE")
        RADIX_CHARS.put(16, RADIX_CHARS[15] + "fF")
    }

    private val EMPTY_ASSOC = mapToAssoc(mutableMapOf<Nothing, Nothing>())

    /* Check if digit is valid for a number in a specific radix */
    fun isValidForRadix(c: Char, radix: Int) = RADIX_CHARS[radix]!!.indexOf(c) > -1

    /* Coerce to DECIMAL64 context if one of the numbers has non-zero scale */
    fun getMathContext(first: BigDecimal, second: BigDecimal): MathContext {
        return if (first.scale() > 0 || second.scale() > 0) MathContext.DECIMAL64 else MathContext.UNLIMITED
    }

    // FIXME Simplify and cleanup!
    /* Check if string represents a valid number and process it */
    fun preProcessNumber(number: String, exactness: Char?, radix: Int): Any? {
        /* First check if it is a special number */
        val special = SPECIAL_NUMBERS[number]
        if (special != null) {
            return special
        }
        /* Check if that is a complex number (ends with `i` or `I`) */
        val last = number[number.length - 1]
        if (last == 'i' || last == 'I') {
            return processComplexNumber(number, exactness, radix)
        }
        /* Multiple decimal points are not allowed*/
        if (number.indexOf('.') != number.lastIndexOf('.')) {
            return Symbol.intern(number)
        }

        /* Read exponent mark if present */
        var exactness = exactness
        val exponentPattern      = if (radix == 16) EXPONENT16_PATTERN else EXPONENT_PATTERN
        val exponentMarksPattern = if (radix == 16) EXPONENT16_MARKS_PATTERN else EXPONENT_MARKS_PATTERN
        var exp: Long? = null
        var n = number
        if (exponentPattern.matcher(number).matches()) {
            val split = number.split(exponentMarksPattern).dropLastWhile { it.isEmpty() }.toTypedArray()
            n = split[0]
            val exponent = split[1]
            try {
                exp = processNumber(exponent, radix, true, false, null) as? Long ?: throw IllegalSyntaxException("read: bad exponent: $number")
            } catch (ex: NumberFormatException) {
                throw IllegalSyntaxException("read: bad exponent: $number")
            }
            exactness = exactness ?: 'i'
        }
        /* Validate sign */
        if (n.lastIndexOf('+') > 0 || n.lastIndexOf('-') > 0) {
            return Symbol.intern(number)
        }

        /* Validate all digits */
        var hasAtLeastOneDigit = false
        for (c in n.toCharArray()) {
            /* Check if char is valid for this radix AND that we don't have # before digits */
            if (c != '/' && !isValidForRadix(c, radix) || c == '#' && !hasAtLeastOneDigit) {
                return Symbol.intern(number)
            }
            /* Check if it is a digit, not a hash/sign char */
            if ("#+-.".indexOf(c) == -1) {
                hasAtLeastOneDigit = true
            }
        }
        if (!hasAtLeastOneDigit) {
            return Symbol.intern(number)
        }

        if (n.indexOf('#') > -1) {
            if (HASH_PATTERN.matcher(n).matches()) {
                n = n.replace('#', '0')
                exactness = exactness ?: 'i'
            } else {
                return Symbol.intern(number)
            }
        }

        /* Check if it is a rational number and if it is valid */
        val slashIndex = n.indexOf('/')
        if (slashIndex > -1 && (slashIndex != n.lastIndexOf('/') || n.indexOf('.') > -1)) {
            return Symbol.intern(number)
        }

        /* Rational and Integral numbers are exact by default */
        val isIntegral = n.indexOf('.') < 0
        val exact = if (exactness != null) Reader.isExact(exactness) else slashIndex > -1 || isIntegral
        val threshold = RADIX_THRESHOLDS[radix]
        val hasSign = if (n[0] == '-' || n[0] == '+') 1 else 0
        if (slashIndex > -1) {
            val numerator = n.substring(0, slashIndex)
            val denominator = n.substring(slashIndex + 1)
            val useBigNum = numerator.length > threshold!! + hasSign || denominator.length > threshold + hasSign
            return processRationalNumber(numerator, denominator, radix, exact, useBigNum, exp)
        }
        val useBigNum = n.length > threshold!! + hasSign
        return processNumber(n, radix, exact, useBigNum, exp)
    }

    private fun processComplexNumber(number: String, exactness: Char?, radix: Int): Any? {
        /* Assume that we have a complex number and try to parse it */
        val p = Math.max(number.lastIndexOf('+'), number.lastIndexOf('-'))
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
    private fun processNumber(number: String, r: Int?, exact: Boolean, useBigNum: Boolean, exp: Long?): Number? {
        var num = number
        var result: Number?
        val dotPos = num.indexOf('.')
        if (useBigNum) {
            if (dotPos < 0) {
                result = BigInteger(num, r!!)
            } else {
                /* Remove dot */
                num = num.replace(".", "")
                /* Process radix */
                var bigDecimal = if (r == 10) BigDecimal(num) else BigDecimal(BigInteger(num, r!!))
                /* Process radix for a number with decimal point */
                bigDecimal = bigDecimal.divide(BIG_DECIMAL_RADICES[r]!!.pow(num.length - dotPos), MathContext.UNLIMITED)
                if (bigDecimal.stripTrailingZeros().scale() == 0) {
                    bigDecimal = bigDecimal.setScale(1, ROUNDING_MODE)
                }
                result = bigDecimal
            }
        } else {
            if (dotPos < 0) {
                result = num.toLong(r!!)
            } else {
                if (r == 10) {
                    result = num.toDouble()
                } else {
                    /* Remove dot */
                    num = num.replace(".", "")
                    result = num.toLong(r!!) / Math.pow(r.toDouble(), (num.length - dotPos).toDouble())
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
                result = Multiplication(result, Expt.expt(r.toLong(), exp))
            }
        }
        return processExactness(result, exact)
    }

    private fun processExactness(number: Number?, exact: Boolean): Number? {
        if (!exact) {
            return ToInexact.toInexact(number)
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
            return ToExact.toExact(number)
        }
        return number
    }

    /* Parse string into a rational number */
    private fun processRationalNumber(numerator: String, denominator: String, r: Int?, exact: Boolean,
                                      useBigNum: Boolean, exp: Long?): Number? {

        val num = processNumber(numerator, r, true, useBigNum, null)
        val den = processNumber(denominator, r, true, useBigNum, null)
        val number = BigRatio.valueOf(num.toString(), den.toString())
        if (!exact) {
            val result = ToInexact.toInexact(number)
            return if (exp == null) result else Multiplication(result, Expt.expt(r, exp))
        }
        return number
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

    fun toBigRatio(number: Number) = when (number) {
        is BigRatio   -> number
        is BigInteger -> BigRatio.valueOf(toBigInteger(number), BigInteger.ONE)
        is BigComplex -> throw UnsupportedOperationException("undefined for complex!")
        else          -> BigRatio.valueOf(number.toString(), "1")
    }

    fun toBigComplex(number: Number) = when (number) {
        is BigComplex -> number
        else          -> BigComplex.of(number)
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
        is Double      -> o as Double? == Math.floor(o) && o.isFinite()
        else           -> false
    }

    fun isExactInteger(o: Any?) = isExact(o) && isInteger(o)

    fun isZero(o: Any?) = when (o) {
        null          -> false
        is Long       -> ((o as Long?)!!).compareTo(0L) == 0
        is Double     -> Math.signum((o as Double?)!!) == 0.0
        is BigRatio   -> o.signum() == 0
        is BigDecimal -> o.signum() == 0
        is Int        -> Integer.signum((o as Int?)!!) == 0
        is Short      -> Integer.signum((o as Short?)!!.toInt()) == 0
        is Byte       -> Integer.signum((o as Byte?)!!.toInt()) == 0
        is Float      -> Math.signum((o as Float?)!!) == 0f
        is BigInteger -> o.signum() == 0
        else          -> false
    }

    fun isOne(o: Any?) = when (o) {
        null          -> false
        is Long       -> (o as Long?)!!.toInt() == 1
        is Double     -> o == 1
        is BigRatio   -> o.isOne
        is BigDecimal -> o.compareTo(BigDecimal.ONE) == 0
        is Int        -> o as Int? == 1
        is Short      -> (o as Short?)!!.toInt() == 1
        is Byte       -> (o as Byte?)!!.toInt() == 1
        is Float      -> java.lang.Float.floatToRawIntBits((o as Float?)!!) == 1
        is BigInteger -> o.compareTo(BigInteger.ONE) == 0
        else          -> false
    }

    fun isPositive(o: Any?) = when (o) {
        null          -> false
        is Long       -> (o as Long?)!! > 0
        is Double     -> Math.signum((o as Double?)!!) == 1.0
        is BigRatio   -> o.signum() == 1
        is BigDecimal -> o.signum() == 1
        is Int        -> Integer.signum((o as Int?)!!) == 1
        is Short      -> Integer.signum((o as Short?)!!.toInt()) == 1
        is Byte       -> Integer.signum((o as Byte?)!!.toInt()) == 1
        is Float      -> Math.signum((o as Float?)!!) == 1f
        is BigInteger -> o.signum() == 1
        else          -> false
    }

    fun isNegative(o: Any?) = when (o) {
        null          -> false
        is Long       -> ((o as Long?)!!) < 0
        is Double     -> Math.signum((o as Double?)!!) == -1.0
        is BigRatio   -> o.signum() == -1
        is BigDecimal -> o.signum() == -1
        is Int        -> Integer.signum((o as Int?)!!) == -1
        is Short      -> Integer.signum((o as Short?)!!.toInt()) == -1
        is Byte       -> Integer.signum((o as Byte?)!!.toInt()) == -1
        is Float      -> Math.signum((o as Float?)!!) == -1f
        is BigInteger -> o.signum() == -1
        else          -> false
    }

    fun isNonNegative(o: Any) = !isNegative(o)

    fun isExactNonNegativeInteger(o: Any) = isExact(o) && isInteger(o) && isNonNegative(o)

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
        isInexact(other) && isExact(result) -> ToInexact.toInexact(result)
        else -> result
    }

    fun downcastNumber(number: Number): Number {
        return when {
            number is BigRatio && number.isDenominatorEqualToOne -> tryDowncast(number)
            number is BigDecimal -> tryDowncast(number)
            number is BigInteger -> tryDowncast(number)
            else                 -> number
        }
    }

    /**
     * Tries to downcast big number to a smaller type (if possible)
     */
    private fun tryDowncast(number: BigDecimal): Number {
        /* Same checks are performed in longValueExact() method,
         * but we don't want exception to be thrown, just return the number */
        if (!isInteger(number)) {
            return number
        }
        try {
            return number.longValueExact()
        } catch (e: ArithmeticException) {
            /* Down-casting has failed, ignore and cast to BigInteger then */
            return number.toBigInteger()
        }
    }

    /* Try to downcast big number to a smaller type (if possible) */
    private fun tryDowncast(number: BigInteger): Number {
        /* Same checks are performed in longValueExact() method,
         * but we don't want exception to be thrown, just return the number */
        if (number.bitLength() <= 63) {
            return number.toLong()
        }
        if (isInteger(number)) {
            try {
                return number.longValueExact()
            } catch (e: ArithmeticException) {
                /* Down-casting has failed, ignore and return the original number */
            }
        }
        return number
    }

    private fun tryDowncast(bigRatio: BigRatio) = tryDowncast(bigRatio.numerator)

    fun isBitOpSupported(obj: Any): Boolean {
        if (!(obj is Byte || obj is Short || obj is Int || obj is Long)) {
            throw WrongTypeException("bit operation not supported for: ${Writer.write(obj)}")
        }
        return true
    }

    /**
     * Converts any Object to boolean.
     * Returns FALSE only if value is FALSE itself or null.
     * Returns TRUE otherwise.
     */
    fun toBoolean(value: Any?) = value as? Boolean ?: (value != null)

    fun isSeqable(obj: Any?) = obj == null || obj is Iterable<*> || obj is CharSequence || obj is Map<*, *> || obj is Map.Entry<*, *>

    fun isAssoc(obj: Any?) = obj == null || obj is Map<*, *> || obj is Map.Entry<*, *> || obj is IAssoc

    fun toSequence(obj: Any?): Sequence<*> = when (obj) {
        is Iterable<*>     -> obj.asSequence()
        is CharSequence    -> stringIterator(obj).asSequence()
        is Map<*, *>       -> mapIterator(obj).asSequence()
        is Map.Entry<*, *> -> MapEntry(obj).iterator().asSequence()
        null               -> emptyList<Any?>().asSequence()
        else               -> throw IllegalArgumentException("don't know how to create Sequence from ${obj.javaClass}")
    }

    fun toAssoc(obj: Any?): IAssoc = when (obj) {
        is IAssoc           -> obj
        is MutableMap<*, *> -> mapToAssoc(obj)
        is Map.Entry<*, *>  -> MapEntry(obj)
        null                -> EMPTY_ASSOC
        else                -> throw IllegalArgumentException("don't know how to create Map from ${obj.javaClass}")
    }

    private fun mapToAssoc(map: MutableMap<*, *>) = object : IAssoc {
        override fun containsKey(key: Any) = map.containsKey(key)
        override fun getEntry(key: Any) = if (map.containsKey(key)) MapEntry(key, map[key]) else null
        override fun assoc(key: Any, value: Any) = throw UnsupportedOperationException()
    }

    /* Returns String Iterator */
    private fun stringIterator(string: CharSequence?) = object : Iterator<Char> {
        private  var index = 0
        override fun hasNext() = index < string!!.length
        override fun next() = if (!hasNext()) throw NoSuchElementException() else string!![index++]
    }

    private fun mapIterator(map: Map<*, *>) = object : Iterator<MapEntry> {
        private  val iterator = map.iterator()
        override fun hasNext() = iterator.hasNext()
        override fun next() = MapEntry(iterator.next())
    }

    /**
     * Up-cast two numbers to the same type
     */
    fun upcast(f: Number?, s: Number?): Array<Number> {
        f!!
        s!!
        when {
            f.javaClass == s.javaClass   -> return arrayOf(f, s)
            !isFinite(f) || !isFinite(s) -> return arrayOf(f, s)
            isInexact(f) || isInexact(s) -> return when {
                f is BigComplex || s is BigComplex -> arrayOf(toBigComplex(f), toBigComplex(s))
                f is BigRatio   || s is BigRatio   -> arrayOf(f.toDouble(), s.toDouble())
                f is BigDecimal || s is BigDecimal -> arrayOf(toBigDecimal(f), toBigDecimal(s))
                f is BigInteger || s is BigInteger -> arrayOf(toBigDecimal(f), toBigDecimal(s))
                f is Double     || s is Double     -> arrayOf(f.toDouble(), s.toDouble())
                f is Float      || s is Float      -> arrayOf(f.toFloat(), s.toFloat())
                else                               -> arrayOf(f, s)
            }
            else -> return when {
                f is BigComplex || s is BigComplex -> arrayOf(toBigComplex(f), toBigComplex(s))
                f is BigRatio   || s is BigRatio   -> arrayOf(toBigRatio(f), toBigRatio(s))
                f is BigDecimal || s is BigDecimal -> arrayOf(toBigDecimal(f), toBigDecimal(s))
                f is BigInteger || s is BigInteger -> arrayOf(toBigInteger(f), toBigInteger(s))
                f is Long       || s is Long       -> arrayOf(f.toLong(), s.toLong())
                f is Int        || s is Int        -> arrayOf(f.toInt(), s.toInt())
                f is Short      || s is Short      -> arrayOf(f.toShort(), s.toShort())
                f is Byte       || s is Byte       -> arrayOf(f.toByte(), s.toByte())
                else                               -> arrayOf(f, s)
            }
        }
    }
}
