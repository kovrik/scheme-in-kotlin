package core.procedures.characters

import core.procedures.AFn
import core.procedures.FnArgs

import java.util.function.Function

class CharProc private constructor(override val name: String, private val function: Function<Char?, Any>) :
        AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Char::class.javaObjectType))) {

    companion object {
        val CHAR_TO_INTEGER = CharProc("char->integer", Function<Char?, Any> { (it as Char).toLong() })
        val CHAR_UPCASE     = CharProc("char-upcase",   Function<Char?, Any> { Character.toUpperCase(it as Char) })
        val CHAR_DOWNCASE   = CharProc("char-downcase", Function<Char?, Any> { Character.toLowerCase(it as Char) })
    }

    override operator fun invoke(arg: Any?) = function.apply(arg as Char?)
}
