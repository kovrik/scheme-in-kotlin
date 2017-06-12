package core.procedures.interop

import core.procedures.AFn

open class PrimitiveNumberType private constructor(override val name: String) :
        AFn<Number?, Number>(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    companion object {
        val BYTE   = object : PrimitiveNumberType("byte")   { override operator fun invoke(arg: Number?) = arg!!.toByte() }
        val SHORT  = object : PrimitiveNumberType("short")  { override operator fun invoke(arg: Number?) = arg!!.toShort() }
        val INT    = object : PrimitiveNumberType("int")    { override operator fun invoke(arg: Number?) = arg!!.toInt() }
        val LONG   = object : PrimitiveNumberType("long")   { override operator fun invoke(arg: Number?) = arg!!.toLong() }
        val FLOAT  = object : PrimitiveNumberType("float")  { override operator fun invoke(arg: Number?) = arg!!.toFloat() }
        val DOUBLE = object : PrimitiveNumberType("double") { override operator fun invoke(arg: Number?) = arg!!.toDouble() }
    }

    override val isPure = true
}
