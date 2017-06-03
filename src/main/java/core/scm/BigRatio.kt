package core.scm

import core.procedures.math.Division
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

        fun valueOf(numerator: String, denominator: String): BigRatio {
            return valueOf(parseBigInteger(numerator), parseBigInteger(denominator))
        }

        fun valueOf(numerator: BigInteger, denominator: BigInteger): BigRatio {
            return when {
                BigInteger.ZERO == denominator -> throw ArithmeticException("Division by zero")
                BigInteger.ZERO == numerator -> ZERO
                BigInteger.ONE  == numerator && BigInteger.ONE == denominator -> ONE
                BigInteger.ONE  == denominator -> BigRatio(numerator)
                else -> BigRatio(numerator, denominator)
            }
        }

        private fun parseBigInteger(number: String) = CONSTANTS.getOrDefault(number, BigInteger(number))
    }

    val numerator: BigInteger
    val denominator: BigInteger

    private constructor(numerator: BigInteger) {
        this.numerator = numerator
        this.denominator = BigInteger.ONE
    }

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

    fun toBigDecimal() = Division.safeBigDecimalDivision(BigDecimal(numerator), BigDecimal(denominator))

    fun toBigDecimalInexact(): BigDecimal {
        val bigDecimal = Division.safeBigDecimalDivision(BigDecimal(numerator), BigDecimal(denominator))
        val scale = Math.max(1, bigDecimal.scale())
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

    fun multiply(other: BigRatio) = BigRatio(numerator.multiply(other.numerator), denominator.multiply(other.denominator))

    fun multiply(other: BigInteger) = BigRatio(numerator.multiply(other), denominator)

    operator fun plus(other: BigRatio): BigRatio {
        val numerator = numerator.multiply(other.denominator).add(other.numerator.multiply(denominator))
        val denominator = denominator.multiply(other.denominator)
        return BigRatio(numerator, denominator)
    }

    operator fun minus(other: BigRatio) = plus(other.negate())

    fun negate() = BigRatio(numerator.negate(), denominator)

    fun signum() = numerator.signum() * denominator.signum()

    fun reciprocal() = BigRatio(denominator, numerator)

    fun divide(other: BigRatio) = multiply(other.reciprocal())

    fun max(other: BigRatio) = if (this > other) this else other

    fun min(other: BigRatio) = if (this < other) this else other

    private fun quotient(): BigDecimal {
        val numerator = BigDecimal(numerator)
        val denominator = BigDecimal(denominator)
        return numerator.divide(denominator, 32, RoundingMode.HALF_EVEN)
    }

    override fun compareTo(other: BigRatio): Int {
        return numerator.multiply(other.denominator).compareTo(denominator.multiply(other.numerator))
    }

    override fun hashCode() = this.toString().hashCode()

    override fun equals(other: Any?): Boolean {
        if (other === this) return true
        if (other == null) return false
        if (other.javaClass != this.javaClass) return false
        val b = other as BigRatio?
        return compareTo(b!!) == 0
    }

    override fun toString(): String {
        return if (denominator == BigInteger.ONE) numerator.toString() else numerator.toString() + "/" + denominator
    }

    override fun toInt()    = quotient().toInt()
    override fun toLong()   = quotient().toLong()
    override fun toFloat()  = quotient().toFloat()
    override fun toDouble() = quotient().toDouble()
    override fun toByte()   = quotient().toByte()
    override fun toChar()   = quotient().toChar()
    override fun toShort()  = quotient().toShort()
}
