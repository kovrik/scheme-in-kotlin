package unittests

import core.environment.DefaultEnvironment
import core.environment.Environment
import core.evaluator.Evaluator
import core.reader.StringReader
import core.scm.Symbol

import java.util.Arrays

import org.junit.Assert.assertEquals

abstract class AbstractTest {

    private val reader = StringReader()
    protected val eval = Evaluator()
    protected val env = DefaultEnvironment()

    init {
        /* Eval lib procedures */
        for (proc in env.libraryProcedures) {
            for (p in reader.read(proc)!!) {
                eval.macroexpandAndEvaluate(p, env)
            }
        }
    }

    /* Helper method: evaluates first S-expression */
    protected fun eval(sexp: String, env: Environment): Any? {
        return eval.macroexpandAndEvaluate(reader.readFirst(sexp)!!, env)
    }

    protected fun s(str: String): Symbol {
        return Symbol.intern(str)!!
    }

    protected fun assertAllEqual(expected: Any, forms: Array<String>, env: Environment) {
        Arrays.stream(forms).forEach { form -> assertEquals(form, expected, eval(form, env)) }
    }
}
