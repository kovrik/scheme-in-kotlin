package core.scm

import core.Evaluator
import core.environment.Environment
import core.utils.Utils
import java.util.concurrent.atomic.AtomicBoolean

class LazySeq(private val form: List<Any?>, private val env: Environment, private val evaluator: Evaluator) : Sequence<Any?> {

    private val realized = AtomicBoolean(false)

    private val seq: Sequence<Any?> by lazy {
        when (form.size) {
            1    -> emptySequence<Nothing>()
            else -> Utils.toSequence(evaluator.eval(form.last(), env))
        }
    }

    // FIXME race condition
    override fun iterator(): Iterator<Any?> {
        if (realized.compareAndSet(false, true)) {
            for (i in 1..form.size - 2) { evaluator.eval(form[i], env) }
        }
        return seq.iterator()
    }
}


