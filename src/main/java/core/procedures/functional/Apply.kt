package core.procedures.functional

import core.procedures.AFn
import core.procedures.IFn
import core.scm.Cons
import core.scm.Symbol
import core.scm.Thunk
import core.scm.specialforms.Quote
import core.utils.Utils

class Apply : AFn(name = "apply", minArgs = 2, mandatoryArgsTypes = arrayOf(IFn::class.java, Any::class.java)) {

    override operator fun invoke(vararg args: Any?): Any {
        val sexp = Cons.list(args[0])
        if (args.size > 2) {
            sexp.addAll(args.copyOfRange(1, args.size - 1))
        }
        val last = args[args.size - 1]
        val iterator = Utils.toSequence(last)
        while (iterator.hasNext()) {
            val o = iterator.next()
            if (o is List<*> || o is Symbol) {
                sexp.add(Quote.quote(o))
            } else {
                sexp.add(o)
            }
        }
        return Thunk(sexp)
    }
}
