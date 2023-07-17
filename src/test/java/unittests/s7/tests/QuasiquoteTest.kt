package unittests.s7.tests

import core.exceptions.IllegalSyntaxException
import core.exceptions.UndefinedIdentifierException
import core.scm.Vector
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test
import unittests.AbstractTest

class QuasiquoteTest : AbstractTest() {

    @Test
    fun testQuasiquote() {
        assertEquals(listOf(1L, 2L, 3L), eval("`(1 2 3)"))
        assertEquals(emptyList<Nothing>(), eval("`()"))
        assertEquals(listOf(s("list"), 3L, 4L), eval("`(list ,(+ 1 2) 4)"))
        assertEquals(listOf(1L, 1L, 2L, 4L), eval("`(1 ,@(list 1 2) 4)"))
        assertEquals(listOf(s("a"), 3L, 4L, 5L, 6L, s("b")), eval("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)"))
        assertEquals(listOf(s("a"), listOf(s("quasiquote"), listOf(s("b"), listOf(s("unquote"), s("x")), listOf(s("unquote"), listOf(s("quote"), s("y"))), s("d"))), s("e")),
                eval("(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))"))
        assertEquals(listOf(1L, 2L, 81L, 3L, 4L), eval("`(1 2 ,(* 9 9) 3 4)"))
        assertEquals(listOf(1L, 2L, 3L), eval("`(1 ,(+ 1 1) 3)"))
        assertEquals(listOf(3L), eval("`(,(+ 1 2))"))
        assertEquals(Pair(s("a"), s("b")), eval("`(,'a . ,'b)"))
        assertEquals(s("foo"), eval("`(,@'() . foo)"))
        assertEquals(s("foo"), eval("(quasiquote (,@'() . foo))"))
        assertEquals(listOf(1L, 2L), eval("`(1 , 2)"))
        assertEquals(listOf(1L, 2L, 3L), eval("`(1 ,@ (list 2 3))"))
        assertEquals(listOf(1L), eval("`(1 ,@(list))"))
        assertEquals(listOf(1L), eval("`(1 ,@'())"))
        assertEquals(listOf(1L, 2L), eval("`(1 , ;a comment" + System.lineSeparator() + "2)"))
        assertEquals(listOf(1L, 1L), eval("`(,1 ,1)"))
        assertEquals(listOf(1L, 1L), eval("`(,1 ,`,1)"))
        assertEquals(listOf(1L, 1L), eval("`(,1 ,`,`,1)"))
        assertEquals(s("quote"), eval("(quasiquote quote)"))
        assertEquals(listOf(0L, 1L, 2L, 3L), eval("(let ((x '(1 2 3))) `(0 . ,x))"))
        assertEquals(listOf(0L, listOf(1L, 2L, 3L)), eval("(let ((x '(1 2 3))) `(0 ,x))"))
        assertEquals(listOf(1L, 2L, 3L), eval("(quasiquote (1 2 3))"))
        assertEquals(emptyList<Nothing>(), eval("(quasiquote ())"))
        assertEquals(listOf(s("list"), 3L, 4L), eval("(quasiquote (list ,(+ 1 2) 4))"))
        assertEquals(listOf(1L, 1L, 2L, 4L), eval("(quasiquote (1 ,@(list 1 2) 4))"))
        assertEquals(listOf(s("a"), 3L, 4L, 5L, 6L, s("b")), eval("(quasiquote (a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))"))
        assertEquals(listOf(1L, 2L, 81L, 3L, 4L), eval("(quasiquote (1 2 ,(* 9 9) 3 4))"))
        assertEquals(listOf(1L, 2L, 3L), eval("(quasiquote (1 ,(+ 1 1) 3))"))
        assertEquals(listOf(3L), eval("(quasiquote (,(+ 1 2)))"))
        assertEquals(listOf(1L, 2L), eval("(quasiquote (1 , 2))"))
        assertEquals(listOf(1L, 1L), eval("(quasiquote (,1 ,1))"))
        assertEquals(listOf(1L, 1L), eval("(quasiquote (,1 ,(quasiquote ,1)))"))
        assertEquals(listOf(1L, 1L), eval("(quasiquote (,1 ,(quasiquote ,(quasiquote ,1))))"))
        assertEquals(listOf(s("+")), eval("(let ((x '())) `(+ ,@x)) '(+)"))
        assertEquals(1L, eval("`(,@1)"))
        assertEquals("String", eval("`(,@\"String\")"))
        assertEquals(Vector(arrayOf(1L)), eval("`[,1]"))
        assertEquals(listOf(listOf(s("quasiquote"), listOf(1L, s("unquote"), 2L))), eval("`(`(1 unquote 2))"))
        assertEquals(listOf(s("quasiquote"), Pair(listOf(s("unquote")), 2L)), eval("``(,,@'() . 2)"))
        try {
            eval("`(1 , %(list 2 3))")
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }

        val illegals = arrayOf("`,@#(list 1 2)", ",(1 (unquote 1 2 3))", "`((unquote (+ 1 2) (+3 4)))", "`[unquote 1]",
                               "(quasiquote 1 2)", "`(1 unquote 2 3)")
        for (s in illegals) {
            try {
                eval(s)
                fail(s)
            } catch (e: IllegalSyntaxException) {
                // expected
            }
        }
    }
}
