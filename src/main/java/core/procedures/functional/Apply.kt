package core.procedures.functional

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.procedures.IFn
import core.scm.Symbol
import core.scm.Thunk
import core.scm.specialforms.Quote
import core.utils.Utils

class Apply : AFn<Any?, Any>(name = "apply", arity = AtLeast(2), mandatoryArgsTypes = arrayOf(IFn::class.java, Any::class.java)) {

    override operator fun invoke(args: Array<out Any?>): Any {
        val sexp = mutableListOf(args[0])
        if (args.size > 2) {
            sexp.addAll(args.copyOfRange(1, args.size - 1))
        }
        val last = args[args.size - 1]
        Utils.toSequence(last).forEach {
            when (it) {
                is Collection<*>, is Map<*, *>, is Symbol -> sexp.add(Quote.quote(it))
                else -> sexp.add(it)
            }
        }
        return Thunk(sexp)
    }
}
