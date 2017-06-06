package core.procedures.interop

import core.procedures.AFn
import core.procedures.FnArgs

open class PrimitiveNumberType private constructor(override val name: String) :
        AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    companion object {
        val BYTE   = object : PrimitiveNumberType("byte")   { override operator fun invoke(arg: Any?) = (arg!! as Number).toByte() }
        val SHORT  = object : PrimitiveNumberType("short")  { override operator fun invoke(arg: Any?) = (arg!! as Number).toShort() }
        val INT    = object : PrimitiveNumberType("int")    { override operator fun invoke(arg: Any?) = (arg!! as Number).toInt() }
        val LONG   = object : PrimitiveNumberType("long")   { override operator fun invoke(arg: Any?) = (arg!! as Number).toLong() }
        val FLOAT  = object : PrimitiveNumberType("float")  { override operator fun invoke(arg: Any?) = (arg!! as Number).toFloat() }
        val DOUBLE = object : PrimitiveNumberType("double") { override operator fun invoke(arg: Any?) = (arg!! as Number).toDouble() }
    }

    override val isPure = true
}
