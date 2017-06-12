package core.procedures.keywords

import core.procedures.AFn
import core.scm.Keyword

class KeywordProc : AFn<CharSequence?, Keyword?>(name = "keyword", minArgs = 1, maxArgs = 1,
                                                 mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = if (arg == null) null else Keyword.intern(arg.toString())
}
