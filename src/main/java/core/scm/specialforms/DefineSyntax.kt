package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator

// TODO
enum class DefineSyntax : ISpecialForm {
    DEFINE_SYNTAX;

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        throw UnsupportedOperationException("NOT IMPLEMENTED YET!")
    }

    override fun toString() = "define-syntax"
}
