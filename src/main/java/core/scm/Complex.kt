package core.scm

import core.procedures.math.*
import core.procedures.math.trigonometry.Cos
import core.procedures.math.trigonometry.Sin
import core.utils.Utils
import java.lang.UnsupportedOperationException

import java.math.BigDecimal
import kotlin.math.PI
import kotlin.math.atan
import kotlin.math.max

/**
 * TODO Implement rational Real and Imaginary parts: 1/2+3/4i
 */
class Complex(tre: BigDecimal, tim: BigDecimal) : Number() {

    companion object {
        /* Imaginary unit (i) */
        val I = Complex(BigDecimal.ZERO, BigDecimal.ONE)

        /* Convert Number to BigComplex */
        fun valueOf(number: Number) = number as? Complex ?: Complex(number)

        private val sqrt = Sqrt()
        private val expt = Expt()
        private val exp  = Exp()
        private val log  = Log()
        private val multiplication = Multiplication()
        private val addition = Addition()
        private val sin = Sin()
        private val cos = Cos()
    }

    /* Real part */
    val re: BigDecimal
    /* Imaginary part */
    val im: BigDecimal

    init {
        val minScale = if (tre.scale() > 0 || tim.scale() > 0) 1 else 0
        val reScaleStripped = tre.stripTrailingZeros().scale()
        val imScaleStripped = tim.stripTrailingZeros().scale()
        val reScale = minOf(Utils.DEFAULT_SCALE, max(minScale, reScaleStripped))
        val imScale = minOf(Utils.DEFAULT_SCALE, max(minScale, imScaleStripped))
        this.re = if (reScaleStripped > 0) tre.setScale(reScale, Utils.ROUNDING_MODE).stripTrailingZeros() else tre.setScale(reScale, Utils.ROUNDING_MODE)
        this.im = if (imScaleStripped > 0) tim.setScale(imScale, Utils.ROUNDING_MODE).stripTrailingZeros() else tim.setScale(imScale, Utils.ROUNDING_MODE)
    }

    @JvmOverloads constructor(re: Number, im: Number = BigDecimal.ZERO) : this(Utils.toBigDecimal(re), Utils.toBigDecimal(im))

    /* Addition */
    operator fun plus(other: Number) = when (other) {
        is Complex -> Complex(re + other.re, im + other.im)
        else -> Complex(re + Utils.toBigDecimal(other), im)
    }

    /* Subtraction */
    operator fun minus(other: Number) = when (other) {
        is Complex -> Complex(re - other.re, im - other.im)
        else -> Complex(re - Utils.toBigDecimal(other), im)
    }

    /**
     * Multiplication
     * (a + bi)(c + di) = (ac - bd) + (bc + ad)i
     */
    operator fun times(other: Number) = valueOf(other).let {
        Complex(re * it.re - im * it.im, im * it.re + re * it.im)
    }

    /**
     * Square root of Complex number
     * sqrt(a + bi) = +-(gamma + delta*i)
     * gamma = sqrt((a + sqrt(a*a + b*b))/2)
     * delta = sign(b) * sqrt((-a + (a*a + b*b)/2)
     */
    fun sqrt(): Complex {
        val s = (re * re + im * im).sqrt(Utils.DEFAULT_CONTEXT)
        val gamma = (s + re).divide(Utils.TWO, Utils.DEFAULT_CONTEXT).sqrt(Utils.DEFAULT_CONTEXT)
        val delta = (s - re).divide(Utils.TWO, Utils.DEFAULT_CONTEXT).sqrt(Utils.DEFAULT_CONTEXT).multiply(im.signum().toBigDecimal())
        return Complex(gamma, delta)
    }

    /**
     * Division
     * a + bi     ac + bd       bc - ad
     * ------ =  ----------  + --------- i
     * c + di    c*c + d*d     c*c + d*d
     */
    operator fun div(other: Number): Complex {
        val o = valueOf(other)
        val real  = re * o.re + im * o.im
        val imag  = im * o.re - re * o.im
        val denom = o.re * o.re + o.im * o.im
        return Complex(real.divide(denom, Utils.DEFAULT_CONTEXT), imag.divide(denom, Utils.DEFAULT_CONTEXT))
    }

    /**
     * Exponentiation
     * z1^z2 = (a+bi)^(c+di) =:
     * pow.re := (r^c)*exp(-d*t)*cos(c*t + d*ln(r))
     * pow.im := (r^c)*exp(-d*t)*sin(c*t + d*ln(r))
     * |_____________|    |_____________|
     * A                  B
     * where:
     * r: magnitude(z1)
     * t: angle(z1)
     * c: z2.re
     * d: z2.im
     */
    fun expt(e: Number): Complex {
        val c: BigDecimal
        val d: BigDecimal
        if (e is Complex) {
            c = e.re
            d = e.im
        } else {
            c = Utils.toBigDecimal(e)
            d = BigDecimal.ZERO
        }
        val r = magnitude()
        val t = angle()
        val A = multiplication(expt(r, c), exp(multiplication(t, -d)))
        val B = addition(multiplication(c, t), multiplication(d, log(r)))
        val re = multiplication(A, cos(B))
        val im = multiplication(A, sin(B))
        return Complex(Utils.toBigDecimal(re), Utils.toBigDecimal(im))
    }

    /**
     * Natural logarithm of Complex number
     * lnz = log(a + ib) = log(|a+bi|) + i*arg(a+bi)
     */
    fun log(): Complex = Complex(log(magnitude()), angle())

    /**
     * Magnitude (Absolute value, Modulus) of Complex number
     * r = |z| = |a+bi| = sqrt(a^2 + b^2)
     */
    fun magnitude() = sqrt(re * re + im * im)

    /**
     * Angle (Argument, Phase) of Complex number
     * arg(z) = arg(a+bi) =:
     * atan(b/y),      if x > 0
     * atan(b/y) + pi, if x < 0 and y >= 0
     * atan(b/y) - pi, if x < 0 and y <  0
     * pi/2,           if x = 0 and y >  0
     * -pi/2,          if x = 0 and y <  0
     * undefined,      if x = 0 and y =  0
     */
    fun angle() = when {
        re.signum() == 0 -> when {
            im.signum() > 0 ->  PI / 2
            im.signum() < 0 -> -PI / 2
            else -> throw ArithmeticException("Undefined for 0+0i")
        }
        re.signum() < 0 -> {
            val atan = atan(im.divide(re, Utils.DEFAULT_CONTEXT).toDouble())
            if (im.signum() >= 0) atan + PI else atan - PI
        }
        else -> atan(im.divide(re, Utils.DEFAULT_CONTEXT).toDouble())
    }

    override fun equals(other: Any?) = when {
        this === other -> true
        other == null || javaClass != other.javaClass -> false
        else -> re == (other as Complex).re && im == other.im
    }

    override fun hashCode() = 31 * re.hashCode() + im.hashCode()

    override fun toString() = when {
        im.signum() < 0 -> "$re-${-im}i"
        else            -> "$re+${im}i"
    }

    override fun toByte()   = throw UnsupportedOperationException()
    override fun toChar()   = throw UnsupportedOperationException()
    override fun toDouble() = throw UnsupportedOperationException()
    override fun toFloat()  = throw UnsupportedOperationException()
    override fun toInt()    = throw UnsupportedOperationException()
    override fun toLong()   = throw UnsupportedOperationException()
    override fun toShort()  = throw UnsupportedOperationException()
}
