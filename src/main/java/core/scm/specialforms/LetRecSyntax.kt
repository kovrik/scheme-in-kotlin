package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator

// TODO
enum class LetRecSyntax : ISpecialForm {
    LETREC_SYNTAX;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        throw UnsupportedOperationException("NOT IMPLEMENTED YET!")
    }

    override fun toString(): String {
        return "letrec-syntax"
    }
}
