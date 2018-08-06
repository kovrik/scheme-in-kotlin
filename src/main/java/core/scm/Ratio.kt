package core.scm

import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger
import java.math.MathContext
import java.math.RoundingMode

class Ratio : Number, Comparable<Ratio> {

    companion object {

        val ZERO = Ratio(BigInteger.ZERO)
        val ONE  = Ratio(BigInteger.ONE)

        private val CONSTANTS = hashMapOf("-2" to "-2".toBigInteger(),
                                          "-1" to "-1".toBigInteger(),
                                          "0"  to BigInteger.ZERO,
                                          "1"  to BigInteger.ONE,
                                          "2"  to "2".toBigInteger(),
                                          "10" to BigInteger.TEN)

        fun valueOf(numerator: String, denominator: String) = valueOf(parseBigInteger(numerator), parseBigInteger(denominator))

        fun valueOf(numerator: BigInteger, denominator: BigInteger) = when {
            BigInteger.ZERO == denominator -> throw ArithmeticException("Division by zero")
            BigInteger.ZERO == numerator -> ZERO
            BigInteger.ONE  == numerator && BigInteger.ONE == denominator -> ONE
            BigInteger.ONE  == denominator -> Ratio(numerator)
            else -> Ratio(numerator, denominator)
        }

        private fun parseBigInteger(number: String) = CONSTANTS.getOrDefault(number, number.toBigInteger())
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
            this.numerator = numerator.divide(g).negate()
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

    fun abs() = Ratio(numerator.abs(), denominator.abs())

    /* Rolls back to DEFAULT_CONTEXT if result cannot be represented with UNLIMITED precision */
    private fun safeBigDecimalDivision(num: BigDecimal, den: BigDecimal): BigDecimal = try {
        num.divide(den, Utils.getMathContext(num, den))
    } catch (e: ArithmeticException) {
        num.divide(den, Utils.DEFAULT_CONTEXT)
    }

    fun toBigDecimal() = safeBigDecimalDivision(numerator.toBigDecimal(), denominator.toBigDecimal())

    fun toBigDecimalInexact(): BigDecimal = safeBigDecimalDivision(numerator.toBigDecimal(), denominator.toBigDecimal()).let {
        it.setScale(maxOf(1, it.scale()), Utils.ROUNDING_MODE)
    }

    fun ceiling(): Ratio {
        val round = if (isPositive) RoundingMode.UP else RoundingMode.DOWN
        return Ratio(numerator.toBigDecimal().divide(denominator.toBigDecimal(), round).toBigInteger())
    }

    fun floor(): Ratio {
        val round = if (isPositive) RoundingMode.DOWN else RoundingMode.UP
        return Ratio(numerator.toBigDecimal().divide(denominator.toBigDecimal(), round).toBigInteger())
    }

    fun round() = Ratio(toBigDecimal().setScale(0, Utils.ROUNDING_MODE).toBigInteger())

    fun truncate() = if (isNegative) ceiling() else floor()

    operator fun times(other: Ratio) = Ratio(numerator.multiply(other.numerator), denominator.multiply(other.denominator))

    operator fun times(other: BigInteger) = Ratio(numerator.multiply(other), denominator)

    operator fun plus(other: Ratio): Ratio {
        val numerator = numerator.multiply(other.denominator).add(other.numerator.multiply(denominator))
        val denominator = denominator.multiply(other.denominator)
        return Ratio(numerator, denominator)
    }

    operator fun minus(other: Ratio) = plus(-other)

    operator fun div(other: Ratio) = times(other.reciprocal())

    operator fun rem(other: Ratio) = this.toBigDecimal() % other.toBigDecimal()

    operator fun unaryMinus() = Ratio(numerator.negate(), denominator)

    fun signum() = numerator.signum() * denominator.signum()

    fun reciprocal() = Ratio(denominator, numerator)

    private fun quotient() = numerator.toBigDecimal().divide(denominator.toBigDecimal(), MathContext.DECIMAL128)

    override fun compareTo(other: Ratio) = numerator.multiply(other.denominator).compareTo(denominator.multiply(other.numerator))

    override fun hashCode() = this.toString().hashCode()

    override fun equals(other: Any?) = when {
        other === this -> true
        other == null -> false
        other.javaClass != this.javaClass -> false
        else -> compareTo(other as Ratio) == 0
    }

    override fun toString() = "$numerator/$denominator"

    override fun toInt()    = quotient().toInt()
    override fun toLong()   = quotient().toLong()
    override fun toFloat()  = quotient().toFloat()
    override fun toDouble() = quotient().toDouble()
    override fun toByte()   = quotient().toByte()
    override fun toChar()   = quotient().toChar()
    override fun toShort()  = quotient().toShort()
}
