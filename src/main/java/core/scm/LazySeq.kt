package core.scm

import core.Evaluator
import core.environment.Environment
import core.utils.Utils
import core.utils.cached
import java.util.concurrent.atomic.AtomicBoolean

class LazySeq(private val form: List<Any?>, private val env: Environment, private val evaluator: Evaluator) : Sequence<Any?> {

    private val realized = AtomicBoolean(false)

    fun isRealized() = realized.get()

    // TODO Check if .cached() is OK here
    override fun iterator() = seq.cached().iterator()

    // TODO is it thread-safe? What if it throws an exception?
    private val seq: Sequence<Any?> by lazy {
        if (realized.compareAndSet(false, true)) {
            for (i in 1..form.size - 2) { evaluator.eval(form[i], env) }
        }
        when (form.size) {
            1    -> emptySequence<Nothing>()
            else -> Utils.toSequence(evaluator.eval(form.last(), env))
        }
    }
}


