package core.procedures.characters

import core.procedures.AFn

class CharProc private constructor(override val name: String, private val function: (Char) -> Any) :
        AFn<Char?, Any?>(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf(Char::class.javaObjectType)) {

    companion object {
        val CHAR_TO_INTEGER = CharProc("char->integer", Char::toLong)
        val CHAR_UPCASE     = CharProc("char-upcase",   Char::toUpperCase)
        val CHAR_DOWNCASE   = CharProc("char-downcase", Char::toLowerCase)
    }

    override operator fun invoke(arg: Char?) = function(arg!!)
}
