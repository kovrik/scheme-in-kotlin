package core.scm.specialforms

import core.Evaluator
import core.scm.Symbol

abstract class SpecialForm(val name: String) {

    internal val symbol = Symbol.intern(name)

    open fun eval(form: List<Any?>, evaluator: Evaluator): Any? = TODO(name)

    override fun toString() = name
}
