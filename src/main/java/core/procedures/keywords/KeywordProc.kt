package core.procedures.keywords

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Keyword

class KeywordProc : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name: String
        get() = "keyword"

    override operator fun invoke(arg: Any?): Keyword? {
        return if (arg == null) null else Keyword.intern(arg.toString())
    }
}
