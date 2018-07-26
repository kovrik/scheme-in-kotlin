package core.scm

import core.Evaluator
import core.environment.Environment
import core.utils.Utils
import core.utils.cached

class LazySeq(private val form: List<Any?>, private val env: Environment, private val evaluator: Evaluator) : Sequence<Any?> {

    internal var realized = false

    override fun iterator() = seq.cached().iterator()

    private val seq: Sequence<Any?> by lazy {
        realized = true
        when (form.size) {
            1 -> emptySequence<Nothing>()
            else -> (1..form.size - 2).forEach {
                evaluator.eval(form[it], env)
            }.let { Utils.toSequence(evaluator.eval(form.last(), env)) }
        }
    }
}
