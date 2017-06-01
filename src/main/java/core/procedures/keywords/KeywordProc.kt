package core.procedures.keywords

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Keyword

class KeywordProc : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(CharSequence::class.java))) {

    override val name = "keyword"
    override operator fun invoke(arg: Any?) = if (arg == null) null else Keyword.intern(arg.toString())
}
