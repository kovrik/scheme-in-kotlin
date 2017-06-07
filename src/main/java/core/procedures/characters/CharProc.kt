package core.procedures.characters

import core.procedures.AFn
import core.procedures.FnArgs

class CharProc private constructor(override val name: String, private val function: (Char?) -> Any) :
        AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Char::class.javaObjectType))) {

    companion object {
        val CHAR_TO_INTEGER = CharProc("char->integer", { it!!.toLong() } )
        val CHAR_UPCASE     = CharProc("char-upcase",   { it!!.toUpperCase() } )
        val CHAR_DOWNCASE   = CharProc("char-downcase", { it!!.toLowerCase() } )
    }

    override operator fun invoke(arg: Any?) = function(arg as Char?)
}
