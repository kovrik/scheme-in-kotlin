package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.IFn
import core.scm.Thunk
import core.scm.Type
import core.utils.Utils
import core.scm.specialforms.And

// (define (every? pred coll)
//                   (cond
//                    ((empty? coll) true)
//                    ((pred (first coll)) (every? pred (rest coll)))
//                    (else false))
class Every : AFn<Any?, Any?>(
    name = "every?", isPure = true, arity = Exactly(2),
    mandatoryArgsTypes = arrayOf(IFn::class.java, Type.Seqable::class.java)
) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any = Utils.toSequence(arg2).apply {
        return@invoke when (any()) {
            true -> Thunk(listOf(And, listOf(arg1, firstOrNull()), listOf(this@Every, arg1, drop(1))))
            false -> true
        }
    }
}
