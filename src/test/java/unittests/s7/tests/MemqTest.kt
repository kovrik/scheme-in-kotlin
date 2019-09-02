package unittests.s7.tests

import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test
import unittests.AbstractTest

class MemqTest : AbstractTest() {

    @Test
    fun testMemq() {
        assertEquals(listOf(s("a"), s("b"), s("c")), (eval("(memq 'a '(a b c))", env) as Sequence<*>).toList())
        assertEquals(listOf(s("a"), s("b"), s("c")), (eval("(memq 'a (list 'a 'b 'c))", env) as Sequence<*>).toList())
        assertEquals(listOf(s("b"), s("c")), (eval("(memq 'b '(a b c))", env) as Sequence<*>).toList())
        assertEquals(false, eval("(memq 'a '(b c d))", env))
        assertEquals(false, eval("(memq (list 'a) '(b (a) c))", env))
        assertEquals(listOf(s("a"), s("c"), s("a"), s("d"), s("a")), (eval("(memq 'a '(b a c a d a))", env) as Sequence<*>).toList())
        assertEquals(listOf<Any?>(false, 2L), (eval("(memq #f '(1 a #t \"hi\" #f 2))", env) as Sequence<*>).toList())
        assertEquals(false, eval("(memq eq? (list 2 eqv? 2))", env))
        assertEquals(listOf(6L), (eval("(memq 6 (memq 5 (memq 4 (memq 3 (memq 2 (memq 1 '(1 2 3 4 5 6)))))))", env) as Sequence<*>).toList())
        assertEquals(Pair(s("a"), s("b")), eval("(memq 'a (cons 'a 'b))", env))
        assertEquals(Pair(s("a"), Pair(s("b"), s("c"))), eval("(memq 'a (cons 'a (cons 'b 'c)))", env))
        assertEquals(Pair(s("b"), s("c")), eval("(memq 'b (cons 'a (cons 'b 'c)))", env))
        assertEquals(listOf(emptyList<Nothing>(), 3L), (eval("(memq '() '(1 () 3))", env) as Sequence<*>).toList())
        assertEquals(false, eval("(memq '() '(1 2))", env))
        assertEquals(listOf(s("a"), s("b"), s("c")), (eval("(memq 'a '(c d a b c))", env) as Sequence<*>).toList())
        assertEquals(false, eval("(memq 'a '(c d f b c))", env))
        assertEquals(false, eval("(memq 'a '())", env))
        assertEquals(Pair(s("a"), Pair(s("b"), s("c"))), eval("(memq 'a (cons 'c (cons 'd (cons 'a (cons 'b 'c)))))", env))
        assertEquals(false, eval("(memq #f '(1 \"hi\" #t))", env))
        assertEquals(false, eval("(memq '() '())", env))
        assertEquals(false, eval("(memq '() (list))", env))
        assertEquals(listOf(listOf<Any>() as Any), (eval("(memq '() (list '()))", env) as Sequence<*>).toList())
        assertEquals(listOf(Pair(1L, 2L), Pair(3L, 4L)), (eval("(let ((x (cons 1 2))) (memq x (list x (cons 3 4))))", env) as Sequence<*>).toList())
        assertEquals(true, eval("(pair? (let ((x (lambda () 1))) (memq x (list 1 2 x 3))))", env))
        assertEquals(listOf(s("a"), s("a"), s("a")), (eval("(memq 'a '(a a a))", env) as Sequence<*>).toList())
        assertEquals(listOf(s("a"), s("a")), (eval("(memq 'a '(b a a))", env) as Sequence<*>).toList())
        assertEquals(listOf<Any?>("hi", 2L), (eval("(memq \"hi\" '(1 \"hi\" 2))", env) as Sequence<*>).toList())
        assertEquals(listOf<Any?>('a', 2L), (eval("(memq #\\a '(1 #f #\\a 2))", env) as Sequence<*>).toList())
        try {
            eval("(memq 'c (cons 'a (cons 'b 'c)))", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("memq: wrong type argument in position 2 (expecting list): (a b . c)", e.message)
        }
        try {
            eval("(memq 'a (cons 'c (cons 'd (cons 'f (cons 'b 'c))))))", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("memq: wrong type argument in position 4 (expecting list): (c d f b . c)", e.message)
        }
    }
}
