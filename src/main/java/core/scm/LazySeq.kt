package core.scm

import core.Evaluator
import core.utils.Utils

class LazySeq(private val form: List<Any?>, private val evaluator: Evaluator) : Sequence<Any?> {

    internal var realized = false

    override fun iterator() = seq.iterator()

    private val seq: Sequence<Any?> by lazy {
        realized = true
        when (form.size) {
            1 -> emptySequence<Nothing>()
            else -> (1..form.size - 2).forEach {
                evaluator.eval(form[it])
            }.let { Utils.toSequence(evaluator.eval(form.last())) }
        }
    }
}
