package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.IFn
import core.scm.Thunk
import core.scm.Type
import core.utils.Utils
import core.scm.specialforms.Or

// (define (some pred coll)
//            (if (empty? coll)
//              null
//              (or (pred (first coll)) (some pred (rest coll)))))
class Some : AFn<Any?, Any?>(
    name = "some", isPure = true, arity = Exactly(2),
    mandatoryArgsTypes = arrayOf(IFn::class.java, Type.Seqable::class.java)
) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val seq = Utils.toSequence(arg2)
        return when (seq.none()) {
            true -> null
            else -> Thunk(listOf(Or, listOf(arg1, seq.first()), listOf(this, arg1, seq.drop(1))))
        }
    }
}
