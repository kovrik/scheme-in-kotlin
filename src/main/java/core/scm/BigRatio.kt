package core.scm

import core.procedures.math.Division
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger
import java.math.RoundingMode

class BigRatio : Number, Comparable<BigRatio> {

    companion object {

        val ZERO = BigRatio(BigInteger.ZERO)
        val ONE = BigRatio(BigInteger.ONE)

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

        private fun parseBigInteger(number: String): BigInteger {
            return CONSTANTS.getOrDefault(number, BigInteger(number))
        }
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

    fun abs(): BigRatio {
        return BigRatio(numerator.abs(), denominator.abs())
    }

    fun toBigDecimal(): BigDecimal {
        return Division.safeBigDecimalDivision(BigDecimal(numerator), BigDecimal(denominator))
    }

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

    fun round(): BigRatio {
        val number = toBigDecimal()
        val round = number.setScale(0, Utils.ROUNDING_MODE)
        return BigRatio(round.toBigInteger())
    }

    fun truncate(): BigRatio {
        return if (isNegative) ceiling() else floor()
    }

    override fun toString(): String {
        return if (denominator == BigInteger.ONE) numerator.toString() else numerator.toString() + "/" + denominator
    }

    override fun compareTo(other: BigRatio): Int {
        return this.numerator.multiply(other.denominator).compareTo(this.denominator.multiply(other.numerator))
    }

    override fun equals(other: Any?): Boolean {
        if (other === this) return true
        if (other == null) return false
        if (other.javaClass != this.javaClass) return false
        val b = other as BigRatio?
        return compareTo(b!!) == 0
    }

    override fun hashCode(): Int {
        return this.toString().hashCode()
    }

    fun multiply(other: BigRatio): BigRatio {
        return BigRatio(this.numerator.multiply(other.numerator), this.denominator.multiply(other.denominator))
    }

    fun multiply(other: BigInteger): BigRatio {
        return BigRatio(this.numerator.multiply(other), this.denominator)
    }

    operator fun plus(other: BigRatio): BigRatio {
        val numerator = this.numerator.multiply(other.denominator).add(other.numerator.multiply(this.denominator))
        val denominator = this.denominator.multiply(other.denominator)
        return BigRatio(numerator, denominator)
    }

    fun negate(): BigRatio {
        return BigRatio(numerator.negate(), denominator)
    }

    operator fun minus(other: BigRatio): BigRatio {
        return this.plus(other.negate())
    }

    fun reciprocal(): BigRatio {
        return BigRatio(denominator, numerator)
    }

    fun divide(other: BigRatio): BigRatio {
        return this.multiply(other.reciprocal())
    }

    fun divide(other: BigInteger): BigRatio {
        return BigRatio(this.numerator, this.denominator.multiply(other))
    }

    private fun quotient(): BigDecimal {
        val numerator = BigDecimal(this.numerator)
        val denominator = BigDecimal(this.denominator)
        return numerator.divide(denominator, 32, RoundingMode.HALF_EVEN)
    }

    override fun toInt(): Int {
        return quotient().toInt()
    }

    override fun toLong(): Long {
        return quotient().toLong()
    }

    override fun toFloat(): Float {
        return quotient().toFloat()
    }

    override fun toDouble(): Double {
        return quotient().toDouble()
    }

    override fun toByte(): Byte {
        return quotient().toByte()
    }

    override fun toChar(): Char {
        return quotient().toChar()
    }

    override fun toShort(): Short {
        return quotient().toShort()
    }

    fun signum(): Int {
        return numerator.signum() * denominator.signum()
    }
}
