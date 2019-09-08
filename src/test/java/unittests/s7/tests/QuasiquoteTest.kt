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
        assertEquals(listOf(1L, 2L, 3L), eval("`(1 2 3)", env))
        assertEquals(emptyList<Nothing>(), eval("`()", env))
        assertEquals(listOf(s("list"), 3L, 4L), eval("`(list ,(+ 1 2) 4)", env))
        assertEquals(listOf(1L, 1L, 2L, 4L), eval("`(1 ,@(list 1 2) 4)", env))
        assertEquals(listOf(s("a"), 3L, 4L, 5L, 6L, s("b")), eval("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", env))
        assertEquals(listOf(s("a"), listOf(s("quasiquote"), listOf(s("b"), listOf(s("unquote"), s("x")), listOf(s("unquote"), listOf(s("quote"), s("y"))), s("d"))), s("e")),
                eval("(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))", env))
        assertEquals(listOf(1L, 2L, 81L, 3L, 4L), eval("`(1 2 ,(* 9 9) 3 4)", env))
        assertEquals(listOf(1L, 2L, 3L), eval("`(1 ,(+ 1 1) 3)", env))
        assertEquals(listOf(3L), eval("`(,(+ 1 2))", env))
        assertEquals(Pair(s("a"), s("b")), eval("`(,'a . ,'b)", env))
        assertEquals(s("foo"), eval("`(,@'() . foo)", env))
        assertEquals(s("foo"), eval("(quasiquote (,@'() . foo))", env))
        assertEquals(listOf(1L, 2L), eval("`(1 , 2)", env))
        assertEquals(listOf(1L, 2L, 3L), eval("`(1 ,@ (list 2 3))", env))
        assertEquals(listOf(1L), eval("`(1 ,@(list))", env))
        assertEquals(listOf(1L), eval("`(1 ,@'())", env))
        assertEquals(listOf(1L, 2L), eval("`(1 , ;a comment" + System.lineSeparator() + "2)", env))
        assertEquals(listOf(1L, 1L), eval("`(,1 ,1)", env))
        assertEquals(listOf(1L, 1L), eval("`(,1 ,`,1)", env))
        assertEquals(listOf(1L, 1L), eval("`(,1 ,`,`,1)", env))
        assertEquals(s("quote"), eval("(quasiquote quote)", env))
        assertEquals(listOf(0L, 1L, 2L, 3L), eval("(let ((x '(1 2 3))) `(0 . ,x))", env))
        assertEquals(listOf(0L, listOf(1L, 2L, 3L)), eval("(let ((x '(1 2 3))) `(0 ,x))", env))
        assertEquals(listOf(1L, 2L, 3L), eval("(quasiquote (1 2 3))", env))
        assertEquals(emptyList<Nothing>(), eval("(quasiquote ())", env))
        assertEquals(listOf(s("list"), 3L, 4L), eval("(quasiquote (list ,(+ 1 2) 4))", env))
        assertEquals(listOf(1L, 1L, 2L, 4L), eval("(quasiquote (1 ,@(list 1 2) 4))", env))
        assertEquals(listOf(s("a"), 3L, 4L, 5L, 6L, s("b")), eval("(quasiquote (a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))", env))
        assertEquals(listOf(1L, 2L, 81L, 3L, 4L), eval("(quasiquote (1 2 ,(* 9 9) 3 4))", env))
        assertEquals(listOf(1L, 2L, 3L), eval("(quasiquote (1 ,(+ 1 1) 3))", env))
        assertEquals(listOf(3L), eval("(quasiquote (,(+ 1 2)))", env))
        assertEquals(listOf(1L, 2L), eval("(quasiquote (1 , 2))", env))
        assertEquals(listOf(1L, 1L), eval("(quasiquote (,1 ,1))", env))
        assertEquals(listOf(1L, 1L), eval("(quasiquote (,1 ,(quasiquote ,1)))", env))
        assertEquals(listOf(1L, 1L), eval("(quasiquote (,1 ,(quasiquote ,(quasiquote ,1))))", env))
        assertEquals(listOf(s("+")), eval("(let ((x '())) `(+ ,@x)) '(+)", env))
        assertEquals(1L, eval("`(,@1)", env))
        assertEquals("String", eval("`(,@\"String\")", env))
        assertEquals(Vector(arrayOf(1L)), eval("`[,1]", env))
        assertEquals(listOf(listOf(s("quasiquote"), listOf(1L, s("unquote"), 2L))), eval("`(`(1 unquote 2))", env))
        assertEquals(listOf(s("quasiquote"), Pair(listOf(s("unquote")), 2L)), eval("``(,,@'() . 2)", env))
        try {
            eval("`(1 , %(list 2 3))", env)
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }

        val illegals = arrayOf("`,@#(list 1 2)", ",(1 (unquote 1 2 3))", "`((unquote (+ 1 2) (+3 4)))", "`[unquote 1]",
                               "(quasiquote 1 2)", "`(1 unquote 2 3)")
        for (s in illegals) {
            try {
                eval(s, env)
                fail(s)
            } catch (e: IllegalSyntaxException) {
                // expected
            }
        }
    }
}
