package unittests.s7.tests

import core.exceptions.ArityException
import core.exceptions.IllegalSyntaxException
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test
import unittests.AbstractTest

class IsProcedureTest : AbstractTest() {

    @Test
    fun testIsProcedure() {
        assertEquals(true,  eval("(procedure? car)"))
        assertEquals(true,  eval("(procedure? procedure?)"))
        assertEquals(true,  eval("(procedure? (lambda (x) x))"))
        assertEquals(true,  eval("(fn? car)"))
        assertEquals(true,  eval("(fn? procedure?)"))
        assertEquals(true,  eval("(fn? (lambda (x) x))"))
        assertEquals(true,  eval("(ifn? car)"))
        assertEquals(true,  eval("(ifn? procedure?)"))
        assertEquals(true,  eval("(ifn? (lambda (x) x))"))
        assertEquals(true,  eval("(let ((a (lambda (x) x))) (procedure? a))"))
        assertEquals(true,  eval("(letrec ((a (lambda () (procedure? a)))) (a))"))
        assertEquals(true,  eval("(let () (define (hi) 1) (procedure? hi))"))
        assertEquals(false, eval("(fn?  'a)"))
        assertEquals(true,  eval("(ifn? 'a)"))
        assertEquals(false, eval("(fn?  :a)"))
        assertEquals(true,  eval("(ifn? :a)"))
        assertEquals(false, eval("(fn?  {})"))
        assertEquals(true,  eval("(ifn? {})"))
        assertEquals(false, eval("(fn?  (first {:a 1}))"))
        assertEquals(true,  eval("(ifn? (first {:a 1}))"))
        assertEquals(false, eval("(fn?  [])"))
        assertEquals(true,  eval("(ifn? [])"))
        assertEquals(false, eval("(fn?  '())"))
        assertEquals(false, eval("(ifn? '())"))
        val falses = arrayOf("(procedure? 'car)", "(procedure? '(lambda (x) x))",
                             "(let ((a 1)) (let ((a (lambda () (procedure? a)))) (a)))", "(procedure? 'and)",
                             "(procedure? 'let)", "(procedure? 'quasiquote)", "(procedure? 'cond)", "(procedure? 'do)",
                             "(procedure? 'set!)", "(procedure? \"hi\")", "(procedure? '(1 2))", "(procedure? #(1 2))",
                             "(procedure? {})", "(procedure? (find {1 2 3 4} 1))", "(procedure? [1 2 3])")
        assertAllEqual(false, falses)
        try {
            eval("(procedure? begin)")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("begin: bad syntax in form: begin", e.message)
        }
        try {
            eval("(procedure? lambda)")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("lambda: bad syntax in form: lambda", e.message)
        }
        try {
            eval("(procedure? and)")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("and: bad syntax in form: and", e.message)
        }
        try {
            eval("(procedure? let)")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("let: bad syntax in form: let", e.message)
        }
        try {
            eval("(procedure? quasiquote)")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("quasiquote: bad syntax in form: quasiquote", e.message)
        }
        try {
            eval("(procedure? cond)")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("cond: bad syntax in form: cond", e.message)
        }
        try {
            eval("(procedure? do)")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("do: bad syntax in form: do", e.message)
        }
        try {
            eval("(procedure? set!)")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("set!: bad syntax in form: set!", e.message)
        }
        try {
            eval("(procedure?)")
            fail()
        } catch (e: ArityException) {
            assertEquals("procedure?: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 0)", e.message)
        }
        try {
            eval("(procedure? abs car)")
            fail()
        } catch (e: ArityException) {
            assertEquals("procedure?: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 2)", e.message)
        }
    }
}
