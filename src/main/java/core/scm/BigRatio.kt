package core.scm

import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger
import java.math.RoundingMode

class BigRatio : Number, Comparable<BigRatio> {

    companion object {

        val ZERO = BigRatio(BigInteger.ZERO)
        val ONE  = BigRatio(BigInteger.ONE)

        private val CONSTANTS = hashMapOf(
                "-2" to BigInteger("-2").negate(),
                "-1" to BigInteger.ONE.negate(),
                "0"  to BigInteger.ZERO,
                "1"  to BigInteger.ONE,
                "2"  to BigInteger("2"),
                "10" to BigInteger.TEN
        )

        fun valueOf(numerator: String, denominator: String) = valueOf(parseBigInteger(numerator), parseBigInteger(denominator))

        fun valueOf(numerator: BigInteger, denominator: BigInteger) = when {
            BigInteger.ZERO == denominator -> throw ArithmeticException("Division by zero")
            BigInteger.ZERO == numerator -> ZERO
            BigInteger.ONE  == numerator && BigInteger.ONE == denominator -> ONE
            BigInteger.ONE  == denominator -> BigRatio(numerator)
            else -> BigRatio(numerator, denominator)
        }

        private fun parseBigInteger(number: String) = CONSTANTS.getOrDefault(number, BigInteger(number))
    }

    val numerator: BigInteger
    val denominator: BigInteger

    private constructor(numerator: BigInteger) : this(numerator, BigInteger.ONE)

    private constructor(numerator: BigInteger, denominator: BigInteger) {
        if (BigInteger.ZERO == denominator) {
            throw ArithmeticException("Division by zero")
        }
        // reduce fraction
        val g = numerator.gcd(denominator)
        val den = denominator.divide(g)
        // to ensure invariant that denominator is positive
        if (den.signum() < 0) {
            this.numerator = numerator.negate()
            this.denominator = den.negate()
        } else {
            this.numerator = numerator.divide(g)
            this.denominator = den
        }
    }

    val isDenominatorEqualToOne: Boolean
        get() = BigInteger.ONE.compareTo(denominator) == 0

    val isZero: Boolean
        get() = signum() == 0

    val isOne: Boolean
        get() = compareTo(ONE) == 0

    val isPositive: Boolean
        get() = signum() == 1

    val isNegative: Boolean
        get() = signum() == -1

    fun abs() = BigRatio(numerator.abs(), denominator.abs())

    /* Rolls back to DEFAULT_CONTEXT if result cannot be represented with UNLIMITED precision */
    fun safeBigDecimalDivision(num: BigDecimal, den: BigDecimal): BigDecimal = try {
        num.divide(den, Utils.getMathContext(num, den))
    } catch (e: ArithmeticException) {
        num.divide(den, Utils.DEFAULT_CONTEXT)
    }

    fun toBigDecimal() = safeBigDecimalDivision(BigDecimal(numerator), BigDecimal(denominator))

    fun toBigDecimalInexact(): BigDecimal {
        val bigDecimal = safeBigDecimalDivision(BigDecimal(numerator), BigDecimal(denominator))
        val scale = maxOf(1, bigDecimal.scale())
        return bigDecimal.setScale(scale, Utils.ROUNDING_MODE)
    }

    fun ceiling(): BigRatio {
        val round = if (isPositive) BigDecimal.ROUND_UP else BigDecimal.ROUND_DOWN
        return BigRatio(BigDecimal(numerator).divide(BigDecimal(denominator), round).toBigInteger())
    }

    fun floor(): BigRatio {
        val round = if (isPositive) BigDecimal.ROUND_DOWN else BigDecimal.ROUND_UP
        return BigRatio(BigDecimal(numerator).divide(BigDecimal(denominator), round).toBigInteger())
    }

    fun round() = BigRatio(toBigDecimal().setScale(0, Utils.ROUNDING_MODE).toBigInteger())

    fun truncate() = if (isNegative) ceiling() else floor()

    operator fun times(other: BigRatio) = BigRatio(numerator.multiply(other.numerator), denominator.multiply(other.denominator))

    operator fun times(other: BigInteger) = BigRatio(numerator.multiply(other), denominator)

    operator fun plus(other: BigRatio): BigRatio {
        val numerator = numerator.multiply(other.denominator).add(other.numerator.multiply(denominator))
        val denominator = denominator.multiply(other.denominator)
        return BigRatio(numerator, denominator)
    }

    operator fun minus(other: BigRatio) = plus(-other)

    operator fun div(other: BigRatio) = times(other.reciprocal())

    operator fun unaryMinus() = BigRatio(numerator.negate(), denominator)

    fun signum() = numerator.signum() * denominator.signum()

    fun reciprocal() = BigRatio(denominator, numerator)

    private fun quotient() = BigDecimal(numerator).divide(BigDecimal(denominator), 32, RoundingMode.HALF_EVEN)

    override fun compareTo(other: BigRatio) = numerator.multiply(other.denominator).compareTo(denominator.multiply(other.numerator))

    override fun hashCode() = this.toString().hashCode()

    override fun equals(other: Any?) = when {
        other === this -> true
        other == null -> false
        other.javaClass != this.javaClass -> false
        else -> compareTo(other as BigRatio) == 0
    }

    override fun toString() = if (denominator == BigInteger.ONE) numerator.toString() else "$numerator/$denominator"

    override fun toInt()    = quotient().toInt()
    override fun toLong()   = quotient().toLong()
    override fun toFloat()  = quotient().toFloat()
    override fun toDouble() = quotient().toDouble()
    override fun toByte()   = quotient().toByte()
    override fun toChar()   = quotient().toChar()
    override fun toShort()  = quotient().toShort()
}
