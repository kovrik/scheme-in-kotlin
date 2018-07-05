package core.procedures.keywords

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Keyword

class KeywordProc : AFn<CharSequence?, Keyword?>(name = "keyword", arity = Exactly(1),
                                                 mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = arg?.let { Keyword.intern(arg.toString()) }
}
