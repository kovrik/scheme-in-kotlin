package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator

// TODO
enum class DefineSyntax : ISpecialForm {
    DEFINE_SYNTAX;

    override fun eval(expression: List<*>, env: Environment, evaluator: Evaluator): Any? {
        throw UnsupportedOperationException("NOT IMPLEMENTED YET!")
    }

    override fun toString(): String {
        return "define-syntax"
    }
}
