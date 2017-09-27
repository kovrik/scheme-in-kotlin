package unittests.s7.tests

import core.procedures.cons.ConsProc
import core.scm.Cons
import core.scm.MutableVector
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test
import unittests.AbstractTest

class MemberTest : AbstractTest() {

    private val cons = ConsProc()

    @Test
    fun testMember() {
        assertEquals(listOf(listOf(s("a")), s("c")), eval("(member (list 'a) '(b (a) c))", env))
        assertEquals(listOf("b"), eval("(member \"b\" '(\"a\" \"c\" \"b\"))", env))
        assertEquals(listOf(1L, 4L), eval("(member 1 '(3 2 1 4))", env))
        assertEquals(listOf(1L, 4L), eval("(member 1 (list 3 2 1 4))", env))
        assertEquals(listOf(s("a"), s("b"), s("c"), s("d")), eval("(member 'a '(a b c d))", env))
        assertEquals(listOf(s("b"), s("c"), s("d")), eval("(member 'b '(a b c d))", env))
        assertEquals(listOf(s("c"), s("d")), eval("(member 'c '(a b c d))", env))
        assertEquals(listOf(s("d")), eval("(member 'd '(a b c d))", env))
        assertEquals(false, eval("(member 'e '(a b c d))", env))
        assertEquals(cons(1L, 2L), eval("(member 1 (cons 1 2))", env))
        assertEquals(cons(1L, cons(2L, 3L)), eval("(member 1 '(1 2 . 3))", env))
        assertEquals(cons(2L, 3L), eval("(member 2 '(1 2 . 3))", env))
        assertEquals(false, eval("(member '() '(1 2 3))", env))
        assertEquals(listOf(listOf<Any>() as Any), eval("(member '() '(1 2 ()))", env))
        assertEquals(listOf(MutableVector(), 3L), eval("(member #() '(1 () 2 #() 3))", env))
        assertEquals(listOf(cons(1L, 2L), cons(3L, 4L)), eval("(let ((x (cons 1 2))) (member x (list (cons 1 2) (cons 3 4))))", env))
        assertEquals(listOf(listOf(1L, 2L) as Any), eval("(let ((x (list 1 2))) (member x (list (cons 1 2) (list 1 2))))", env))
        assertEquals(listOf(listOf(s("quote"), s("a")), s("b"), s("c")), eval("(member ''a '('a b c))", env))
        assertEquals(listOf(s("a"), s("a"), s("a")), eval("(member 'a '(a a a)))", env))
        assertEquals(listOf(s("a"), s("a")), eval("(member 'a '(b a a))", env))
        assertEquals(listOf(listOf(3L, 4L) as Any, listOf(4L, 5L) as Any), eval("(member (member 3 '(1 2 3 4)) '((1 2) (2 3) (3 4) (4 5)))", env))
        assertEquals(listOf<Any?>("hi", 2L), eval("(member \"hi\" '(1 \"hi\" 2))", env))
        assertEquals(listOf<Any?>('a', 2L), eval("(member #\\a '(1 #f #\\a 2))", env))
        assertEquals(listOf(MutableVector(arrayOf(1L, 2L, 3L)), MutableVector(arrayOf(1L, 2L))), eval("(let* ((x (vector 1 2 3)) (lst (list 1 \"hi\" x (vector 1 2)))) (member x lst))", env))
        assertEquals(listOf(MutableVector(arrayOf(1L, 2L, 3L)) as Any), eval("(let* ((x (vector 1 2 3)) (lst (list 1 \"hi\" (vector 1 2 3)))) (member x lst))", env))
        assertEquals(listOf(3L), eval("(member 3 . ('(1 2 3)))", env))
        assertEquals(Cons.cons(3L, 4L), eval("(member 3 . ('(1 2 3 . 4)))", env))
        assertEquals(listOf(3L), eval("(member . (3 '(1 2 3)))", env))
        assertEquals(false, eval("(member '(1 2) '(1 2))", env))
        assertEquals(listOf(listOf(1L, 2L) as Any), eval("(member '(1 2) '((1 2)))", env))
        assertEquals(Cons.cons(4L, 5L), eval("(member 4 '(1 2 3 4 . 5))", env))
        try {
            eval("(member 3 '(1 2 . 3))", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("member: wrong type argument in position 2 (expecting list): (1 2 . 3)", e.message)
        }
        try {
            eval("(member 4 '(1 2 . 3))", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("member: wrong type argument in position 2 (expecting list): (1 2 . 3)", e.message)
        }
        try {
            eval("(member 4 '(1 2 3 . 4))", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("member: wrong type argument in position 4 (expecting list): (1 2 3 . 4)", e.message)
        }
    }
}
