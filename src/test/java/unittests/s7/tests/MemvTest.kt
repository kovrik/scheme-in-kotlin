package unittests.s7.tests

import core.exceptions.ArityException
import core.exceptions.IllegalSyntaxException
import core.procedures.cons.ConsProc
import core.scm.Cons.Companion.EMPTY
import core.scm.Cons.Companion.list
import core.scm.MutableVector
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test
import unittests.AbstractTest

class MemvTest : AbstractTest() {

    private val cons = ConsProc()

    @Test
    fun testMemv() {
        assertEquals(list(101L, 102L), eval("(memv 101 '(100 101 102))", env))
        assertEquals(list(101L, 102L), eval("(memv 101 (list 100 101 102))", env))
        assertEquals(list(3.4, 4.5), eval("(memv 3.4 '(1.2 2.3 3.4 4.5))", env))
        assertEquals(false, eval("(memv 3.4 '(1.3 2.5 3.7 4.9))", env))
        assertEquals(false, eval("(memv 1.0 '(1 2 3))", env))
        assertEquals(cons(1L, 2L), eval("(memv 1 (cons 1 2))", env))
        assertEquals(cons(s("a"), cons(s("b"), s("c"))), eval("(memv 'a '(a b . c))", env))
        assertEquals(list(s("c")), eval("(memv 'c '(a b c))", env))
        assertEquals(false, eval("(memv ''a '('a b c))", env))
        assertEquals(false, eval("(let ((x (cons 1 2))) (memv x (list (cons 1 2) (cons 3 4))))", env))
        assertEquals(list(cons(1L, 2L), cons(3L, 4L)), eval("(let ((x (cons 1 2))) (memv x (list x (cons 3 4))))", env))
        assertEquals(list(s("a"), s("a"), s("a")), eval("(memv 'a '(a a a))", env))
        assertEquals(list(s("a"), s("a")), eval("(memv 'a '(b a a))", env))
        assertEquals(list<Any?>("hi", 2L), eval("(memv \"hi\" '(1 \"hi\" 2))", env))
        assertEquals(list<Any?>('a', 2L), eval("(memv #\\a '(1 #f #\\a 2))", env))
        assertEquals(false, eval("(memv #(1) '(1 #(1) 2))", env))
        assertEquals(list(EMPTY, 2L), eval("(memv '() '(1 () 2))", env))
        assertEquals(list(MutableVector(arrayOf(1L, 2L, 3L)), MutableVector(arrayOf(1L, 2L))), eval("(let* ((x (vector 1 2 3)) (lst (list 1 \"hi\" x (vector 1 2)))) (memv x lst))", env))
        assertEquals(false, eval("(let* ((x (vector 1 2 3)) (lst (list 1 \"hi\" (vector 1 2 3)))) (memv x lst))", env))
        try {
            eval("(memv 'asdf '(a b . c))", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("memv: wrong type argument in position 2 (expecting list): (a b . c)", e.message)
        }
        try {
            eval("(memv 'c '(a b . c))", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("memv: wrong type argument in position 2 (expecting list): (a b . c)", e.message)
        }
        try {
            eval("(memv 'a (list 'a 'b . 'c))", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("quote: bad syntax in form: quote", e.message)
        }
        try {
            eval("(memv)", env)
            fail()
        } catch (e: ArityException) {
            assertEquals("memv: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.message)
        }
        try {
            eval("(memv 'a)", env)
            fail()
        } catch (e: ArityException) {
            assertEquals("memv: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 1)", e.message)
        }
        try {
            eval("(memv 'a 'b)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("memv: type mismatch; (expected: List, given: b)", e.message)
        }
    }
}
