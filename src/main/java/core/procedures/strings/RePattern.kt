package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly

import java.util.regex.Pattern

class RePattern : AFn<CharSequence?, Pattern>(name = "re-pattern", isPure = true, arity = Exactly(1),
                                              mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?): Pattern = Pattern.compile(arg!!.toString())
}
