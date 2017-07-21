package unittests.s7.tests

import core.exceptions.ArityException
import core.exceptions.IllegalSyntaxException
import org.junit.Test
import unittests.AbstractTest

import org.junit.Assert.assertEquals
import org.junit.Assert.fail

class IsProcedureTest : AbstractTest() {

    @Test
    fun testIsProcedure() {
        assertEquals(true, eval("(procedure? car)", env))
        assertEquals(true, eval("(procedure? procedure?)", env))
        assertEquals(true, eval("(procedure? (lambda (x) x))", env))
        assertEquals(true, eval("(let ((a (lambda (x) x))) (procedure? a))", env))
        assertEquals(true, eval("(letrec ((a (lambda () (procedure? a)))) (a))", env))
        assertEquals(true, eval("(let () (define (hi) 1) (procedure? hi))", env))
        val falses = arrayOf("(procedure? 'car)", "(procedure? '(lambda (x) x))",
                "(let ((a 1)) (let ((a (lambda () (procedure? a)))) (a)))", "(procedure? 'and)", "(procedure? 'let)",
                "(procedure? 'quasiquote)", "(procedure? 'cond)", "(procedure? 'do)", "(procedure? 'set!)",
                "(procedure? \"hi\")", "(procedure? '(1 2))", "(procedure? #(1 2))", "(procedure? {})",
                "(procedure? (find {1 2 3 4} 1))", "(procedure? [1 2 3])")
        assertAllEqual(false, falses, env)
        try {
            eval("(procedure? begin)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("begin: bad syntax in form: begin", e.message)
        }
        try {
            eval("(procedure? lambda)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("lambda: bad syntax in form: lambda", e.message)
        }
        try {
            eval("(procedure? and)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("and: bad syntax in form: and", e.message)
        }
        try {
            eval("(procedure? let)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("let: bad syntax in form: let", e.message)
        }
        try {
            eval("(procedure? quasiquote)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("quasiquote: bad syntax in form: quasiquote", e.message)
        }
        try {
            eval("(procedure? cond)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("cond: bad syntax in form: cond", e.message)
        }
        try {
            eval("(procedure? do)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("do: bad syntax in form: do", e.message)
        }
        try {
            eval("(procedure? set!)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("set!: bad syntax in form: set!", e.message)
        }
        try {
            eval("(procedure?)", env)
            fail()
        } catch (e: ArityException) {
            assertEquals("procedure?: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 0)", e.message)
        }
        try {
            eval("(procedure? abs car)", env)
            fail()
        } catch (e: ArityException) {
            assertEquals("procedure?: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 2)", e.message)
        }
    }
}
