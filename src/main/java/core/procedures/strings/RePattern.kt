package core.procedures.strings

import core.procedures.AFn

import java.util.regex.Pattern

class RePattern : AFn(name = "re-pattern", isPure = true, minArgs = 1, maxArgs = 1,
                      mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?): Pattern = Pattern.compile(arg!!.toString())
}
