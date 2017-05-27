package unittests.s7.tests

import core.scm.Cons
import core.scm.Cons.Companion.cons
import core.scm.Cons.Companion.list
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test
import unittests.AbstractTest

class MemqTest : AbstractTest() {

    @Test
    fun testMemq() {
        assertEquals(list(s("a"), s("b"), s("c")), eval("(memq 'a '(a b c))", env))
        assertEquals(list(s("a"), s("b"), s("c")), eval("(memq 'a (list 'a 'b 'c))", env))
        assertEquals(list(s("b"), s("c")), eval("(memq 'b '(a b c))", env))
        assertEquals(false, eval("(memq 'a '(b c d))", env))
        assertEquals(false, eval("(memq (list 'a) '(b (a) c))", env))
        assertEquals(list(s("a"), s("c"), s("a"), s("d"), s("a")), eval("(memq 'a '(b a c a d a))", env))
        assertEquals(list<Any?>(false, 2L), eval("(memq #f '(1 a #t \"hi\" #f 2))", env))
        assertEquals(false, eval("(memq eq? (list 2 eqv? 2))", env))
        assertEquals(list(6L), eval("(memq 6 (memq 5 (memq 4 (memq 3 (memq 2 (memq 1 '(1 2 3 4 5 6)))))))", env))
        assertEquals(cons(s("a"), s("b")), eval("(memq 'a (cons 'a 'b))", env))
        assertEquals(cons(s("a"), cons(s("b"), s("c"))), eval("(memq 'a '(a b . c))", env))
        assertEquals(cons(s("b"), s("c")), eval("(memq 'b '(a b . c))", env))
        assertEquals(list(Cons.EMPTY, 3L), eval("(memq '() '(1 () 3))", env))
        assertEquals(false, eval("(memq '() '(1 2))", env))
        assertEquals(list(s("a"), s("b"), s("c")), eval("(memq 'a '(c d a b c))", env))
        assertEquals(false, eval("(memq 'a '(c d f b c))", env))
        assertEquals(false, eval("(memq 'a '())", env))
        assertEquals(cons(s("a"), cons(s("b"), s("c"))), eval("(memq 'a '(c d a b . c))", env))
        assertEquals(false, eval("(memq #f '(1 \"hi\" #t))", env))
        assertEquals(false, eval("(memq '() '())", env))
        assertEquals(false, eval("(memq '() (list))", env))
        assertEquals(list(list<Any>() as Any), eval("(memq '() (list '()))", env))
        assertEquals(list(cons(1L, 2L), cons(3L, 4L)), eval("(let ((x (cons 1 2))) (memq x (list x (cons 3 4))))", env))
        assertEquals(true, eval("(pair? (let ((x (lambda () 1))) (memq x (list 1 2 x 3))))", env))
        assertEquals(list(s("a"), s("a"), s("a")), eval("(memq 'a '(a a a))", env))
        assertEquals(list(s("a"), s("a")), eval("(memq 'a '(b a a))", env))
        assertEquals(list<Any?>("hi", 2L), eval("(memq \"hi\" '(1 \"hi\" 2))", env))
        assertEquals(list<Any?>('a', 2L), eval("(memq #\\a '(1 #f #\\a 2))", env))
        try {
            eval("(memq 'c '(a b . c))", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("memq: wrong type argument in position 2 (expecting list): (a b . c)", e.message)
        }

        try {
            eval("(memq 'a '(c d f b . c))", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("memq: wrong type argument in position 5 (expecting list): (c d f b . c)", e.message)
        }
    }
}
