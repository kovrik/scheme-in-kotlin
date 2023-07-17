package unittests

import core.environment.DefaultEnvironment
import core.Evaluator
import core.reader.StringReader
import core.scm.Symbol
import org.junit.Assert.assertEquals
import java.util.*

abstract class AbstractTest {

    private val reader = StringReader()
    private val environment = DefaultEnvironment()
    private val evaluator = Evaluator(environment).apply {
        with (reader) {
            environment.libraryProcedures.forEach { this@apply.eval(readOne(it)) }
        }
    }

    /* Helper method: evaluates first S-expression */
    protected fun eval(sexp: String) = evaluator.macroexpandAndEvaluate(reader.readOne(sexp)!!)

    protected fun s(str: String) = Symbol.intern(str)

    protected fun assertAllEqual(expected: Any, forms: Array<String>) {
        Arrays.stream(forms).forEach { assertEquals(it, expected, eval(it)) }
    }
}
