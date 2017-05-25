package core.scm

import core.procedures.math.*
import core.procedures.math.trigonometry.Atan
import core.procedures.math.trigonometry.Cos
import core.procedures.math.trigonometry.Sin
import core.utils.Utils
import java.lang.UnsupportedOperationException

import java.math.BigDecimal

/**
 * TODO Implement rational Real and Imaginary parts: 1/2+3/4i
 */
class BigComplex(tre: BigDecimal, tim: BigDecimal) : Number() {

    companion object {
        /* Imaginary unit (i) */
        val I = BigComplex(BigDecimal.ZERO, BigDecimal.ONE)

        /* Convert Number to BigComplex */
        fun of(number: Number): BigComplex {
            return number as? BigComplex ?: BigComplex(number)
        }
    }

    /* real part */
    val re: BigDecimal
    /* imaginary part */
    val im: BigDecimal

    /* Complex number is a zero if both real and imaginary parts are zeroes */
    val isZero: Boolean
        get() = re.signum() == 0 && im.signum() == 0

    init {
        val minScale = if (tre.scale() > 0 || tim.scale() > 0) 1 else 0
        val reScaleStripped = tre.stripTrailingZeros().scale()
        val imScaleStripped = tim.stripTrailingZeros().scale()
        val reScale = Math.min(Utils.DEFAULT_SCALE, Math.max(minScale, reScaleStripped))
        val imScale = Math.min(Utils.DEFAULT_SCALE, Math.max(minScale, imScaleStripped))
        this.re = if (reScaleStripped > 0) tre.setScale(reScale, Utils.ROUNDING_MODE).stripTrailingZeros() else tre.setScale(reScale, Utils.ROUNDING_MODE)
        this.im = if (imScaleStripped > 0) tim.setScale(imScale, Utils.ROUNDING_MODE).stripTrailingZeros() else tim.setScale(imScale, Utils.ROUNDING_MODE)
    }

    @JvmOverloads constructor(re: Number, im: Number = BigDecimal.ZERO) : this(Utils.toBigDecimal(re), Utils.toBigDecimal(im))

    /* Addition */
    operator fun plus(other: Number): BigComplex {
        if (other is BigComplex) {
            return BigComplex(re.add(other.re), im.add(other.im))
        } else {
            return BigComplex(re.add(Utils.toBigDecimal(other)), im)
        }
    }

    /* Subtraction */
    operator fun minus(other: Number): BigComplex {
        if (other is BigComplex) {
            return BigComplex(re.subtract(other.re), im.subtract(other.im))
        } else {
            return BigComplex(re.subtract(Utils.toBigDecimal(other)), im)
        }
    }

    /**
     * Multiplication
     * (a + bi)(c + di) = (ac - bd) + (bc + ad)i
     */
    fun multiply(other: Number): BigComplex {
        val o = of(other)
        val a = this.re
        val b = this.im
        val c = o.re
        val d = o.im
        return BigComplex(a.multiply(c).subtract(b.multiply(d)), b.multiply(c).add(a.multiply(d)))
    }

    /**
     * Square root of Complex number
     * sqrt(a + bi) = +-(gamma + delta*i)
     * gamma = sqrt((a + sqrt(a*a + b*b))/2)
     * delta = sign(b) * sqrt((-a + (a*a + b*b)/2)
     */
    fun sqrt(): BigComplex {
        val a = this.re.toDouble()
        val b = this.im.toDouble()
        val signum = this.im.signum()
        val s = Math.sqrt(a * a + b * b)
        val gamma = Math.sqrt((s + a) / 2)
        val delta = signum * Math.sqrt((s - a) / 2)
        return BigComplex(gamma, delta)
    }

    /**
     * Division
     * a + bi     ac + bd       bc - ad
     * ------ =  ----------  + --------- i
     * c + di    c*c + d*d     c*c + d*d
     */
    fun divide(other: Number): BigComplex {
        val o = of(other)
        val a = this.re
        val b = this.im
        val c = o.re
        val d = o.im
        val real = a.multiply(c).add(b.multiply(d))
        val imag = b.multiply(c).subtract(a.multiply(d))
        val denom = c.multiply(c).add(d.multiply(d))
        return BigComplex(real.divide(denom, Utils.DEFAULT_CONTEXT), imag.divide(denom, Utils.DEFAULT_CONTEXT))
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
    fun expt(e: Number): BigComplex {
        val c: BigDecimal
        val d: BigDecimal
        if (e is BigComplex) {
            c = e.re
            d = e.im
        } else {
            c = Utils.toBigDecimal(e)
            d = BigDecimal.ZERO
        }
        val r = magnitude()
        val t = angle()
        val A = Multiplication(Expt.expt(r, c), Exp.exp(Multiplication(t, d.negate())))
        val B = Addition.add(Multiplication(c, t), Multiplication(d, Log.log(r)))
        val re = Multiplication(A, Cos.cos(B!!))
        val im = Multiplication(A, Sin.sin(B))
        return BigComplex(Utils.toBigDecimal(re), Utils.toBigDecimal(im))
    }

    /**
     * Natural logarithm of Complex number
     * lnz = log(a + ib) = log(|a+bi|) + i*arg(a+bi)
     */
    fun log(): BigComplex {
        val re = Log.log(magnitude())
        val im = angle()
        return BigComplex(re, im)
    }

    /**
     * Magnitude (Absolute value, Modulus) of Complex number
     * r = |z| = |a+bi| = sqrt(a^2 + b^2)
     */
    fun magnitude(): Number {
        val re = re
        val im = im
        return Sqrt.sqrt(Addition.add(re.multiply(re), im.multiply(im)))
    }

    /**
     * Angle (Argument, Phase) of Complex number
     * arg(z) = arg(a+bi) =:
     * atan(b/y),      if x > 0
     * atan(b/y) + pi, if x < 0 and y >= 0
     * atan(b/y) - pi, if x < 0 and y <  0
     * pi/2,          if x = 0 and y >  0
     * -pi/2,          if x = 0 and y <  0
     * undefined,      if x = 0 and y =  0
     */
    fun angle(): Number {
        val re = re
        val im = im
        if (re.signum() == 0) {
            if (im.signum() > 0) {
                return Math.PI / 2
            } else if (im.signum() < 0) {
                return -Math.PI / 2
            } else {
                throw ArithmeticException("Undefined for 0+0i")
            }
        } else if (re.signum() < 0) {
            val atan = Atan.atan(im.divide(re, Utils.DEFAULT_CONTEXT))
            return if (im.signum() >= 0) atan + Math.PI else atan - Math.PI
        } else {
            return Atan.atan(im.divide(re, Utils.DEFAULT_CONTEXT))
        }
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || javaClass != other.javaClass) return false
        val that = other as BigComplex?
        if (re != that!!.re) return false
        return if (true) im == that.im else false
    }

    override fun hashCode(): Int {
        var result = re.hashCode()
        result = 31 * result + (im.hashCode())
        return result
    }

    override fun toString(): String {
        return if (im.signum() < 0) (re.toString() + "-" + im.negate() + "i") else (re.toString() + "+" + im + "i")
    }

    override fun toByte(): Byte {
        throw UnsupportedOperationException()
    }

    override fun toChar(): Char {
        throw UnsupportedOperationException()
    }

    override fun toDouble(): Double {
        throw UnsupportedOperationException()
    }

    override fun toFloat(): Float {
        throw UnsupportedOperationException()
    }

    override fun toInt(): Int {
        throw UnsupportedOperationException()
    }

    override fun toLong(): Long {
        throw UnsupportedOperationException()
    }

    override fun toShort(): Short {
        throw UnsupportedOperationException()
    }
}
