package unittests.s7.tests

import core.exceptions.ArityException
import core.exceptions.IllegalSyntaxException
import core.scm.MutableVector
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test
import unittests.AbstractTest

class MemvTest : AbstractTest() {

    @Test
    fun testMemv() {
        assertEquals(listOf(101L, 102L), (eval("(memv 101 '(100 101 102))") as Sequence<*>).toList())
        assertEquals(listOf(101L, 102L), (eval("(memv 101 (list 100 101 102))") as Sequence<*>).toList())
        assertEquals(listOf(3.4, 4.5), (eval("(memv 3.4 '(1.2 2.3 3.4 4.5))") as Sequence<*>).toList())
        assertEquals(false, eval("(memv 3.4 '(1.3 2.5 3.7 4.9))"))
        assertEquals(false, eval("(memv 1.0 '(1 2 3))"))
        assertEquals(Pair(1L, 2L), eval("(memv 1 (cons 1 2))"))
        assertEquals(Pair(s("a"), Pair(s("b"), s("c"))), (eval("(memv 'a (cons 'a (cons 'b 'c)))")))
        assertEquals(listOf(s("c")), (eval("(memv 'c '(a b c))") as Sequence<*>).toList())
        assertEquals(false, eval("(memv ''a '('a b c))"))
        assertEquals(false, eval("(let ((x (cons 1 2))) (memv x (list (cons 1 2) (cons 3 4))))"))
        assertEquals(listOf(Pair(1L, 2L), Pair(3L, 4L)), (eval("(let ((x (cons 1 2))) (memv x (list x (cons 3 4))))") as Sequence<*>).toList())
        assertEquals(listOf(s("a"), s("a"), s("a")), (eval("(memv 'a '(a a a))") as Sequence<*>).toList())
        assertEquals(listOf(s("a"), s("a")), (eval("(memv 'a '(b a a))") as Sequence<*>).toList())
        assertEquals(listOf<Any?>("hi", 2L), (eval("(memv \"hi\" '(1 \"hi\" 2))") as Sequence<*>).toList())
        assertEquals(listOf<Any?>('a', 2L), (eval("(memv #\\a '(1 #f #\\a 2))") as Sequence<*>).toList())
        assertEquals(false, eval("(memv #(1) '(1 #(1) 2))"))
        assertEquals(listOf<Any?>(emptyList<Nothing>(), 2L), (eval("(memv '() '(1 () 2))") as Sequence<*>).toList())
        assertEquals(listOf(MutableVector(arrayOf(1L, 2L, 3L)), MutableVector(arrayOf(1L, 2L))), (eval("(let* ((x (vector 1 2 3)) (lst (list 1 \"hi\" x (vector 1 2)))) (memv x lst))") as Sequence<*>).toList())
        assertEquals(false, eval("(let* ((x (vector 1 2 3)) (lst (list 1 \"hi\" (vector 1 2 3)))) (memv x lst))"))
        try {
            eval("(memv 'asdf (cons 'a (cons 'b 'c)))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("memv: wrong type argument in position 2 (expecting list): (a b . c)", e.message)
        }
        try {
            eval("(memv 'c (cons 'a (cons 'b 'c)))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("memv: wrong type argument in position 2 (expecting list): (a b . c)", e.message)
        }
        try {
            eval("(memv 'a (list 'a 'b . 'c))")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("quote: bad syntax in form: quote", e.message)
        }
        try {
            eval("(memv)")
            fail()
        } catch (e: ArityException) {
            assertEquals("memv: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.message)
        }
        try {
            eval("(memv 'a)")
            fail()
        } catch (e: ArityException) {
            assertEquals("memv: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 1)", e.message)
        }
        try {
            eval("(memv 'a 'b)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("memv: type mismatch; (expected: Seqable, given: b)", e.message)
        }
    }
}
