package core.procedures.interop

import core.procedures.AFn
import core.procedures.FnArgs

class PrimitiveNumberType private constructor(private val clazz: Class<*>) :
        AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    companion object {
        val BYTE   = PrimitiveNumberType(Byte::class.java)
        val SHORT  = PrimitiveNumberType(Short::class.java)
        val INT    = PrimitiveNumberType(Int::class.java)
        val LONG   = PrimitiveNumberType(Long::class.java)
        val DOUBLE = PrimitiveNumberType(Double::class.java)
        val FLOAT  = PrimitiveNumberType(Float::class.java)
    }

    override val isPure = true
    override val name: String = clazz.simpleName

    /* FIXME Have to box it */
    override operator fun invoke(arg: Any?): Number? {
        val number = arg as Number?
        when (clazz) {
            Byte::class.javaPrimitiveType   -> return number!!.toByte()
            Short::class.javaPrimitiveType  -> return number!!.toShort()
            Int::class.javaPrimitiveType    -> return number!!.toInt()
            Long::class.javaPrimitiveType   -> return number!!.toLong()
            Double::class.javaPrimitiveType -> return number!!.toDouble()
            Float::class.javaPrimitiveType  -> return number!!.toFloat()
            else -> throw IllegalArgumentException("Unknown primitive type: " + clazz)
        }
    }
}
