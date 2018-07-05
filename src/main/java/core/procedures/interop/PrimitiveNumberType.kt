package core.procedures.interop

import core.procedures.AFn
import core.procedures.Arity.Exactly

open class PrimitiveNumberType private constructor(override val name: String, private val function: (Number) -> Number) :
        AFn<Number?, Number>(arity = Exactly(1), isPure = true, mandatoryArgsTypes = arrayOf(Number::class.java)) {

    companion object {
        val BYTE   = PrimitiveNumberType("byte",   Number::toByte)
        val SHORT  = PrimitiveNumberType("short",  Number::toShort)
        val INT    = PrimitiveNumberType("int",    Number::toInt)
        val LONG   = PrimitiveNumberType("long",   Number::toLong)
        val FLOAT  = PrimitiveNumberType("float",  Number::toFloat)
        val DOUBLE = PrimitiveNumberType("double", Number::toDouble)
    }

    override operator fun invoke(arg: Number?) = function(arg!!)
}
