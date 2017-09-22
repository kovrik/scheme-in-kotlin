package core.scm.specialforms

import core.Evaluator
import core.environment.Environment
import core.scm.LazySeq

/* Syntax:
 * (lazy-seq <body> ...)
 */
object LazySeqForm : SpecialForm("lazy-seq") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = LazySeq(form, env, evaluator)
}
