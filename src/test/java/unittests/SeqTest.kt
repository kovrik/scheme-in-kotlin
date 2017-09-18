package unittests

import core.exceptions.WrongTypeException
import core.scm.Cons.Companion.list
import core.scm.MutableVector
import org.junit.Ignore
import org.junit.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class SeqTest : AbstractTest() {

    @Test
    fun testRepeat() {
        assertEquals(list<Any>(), eval("(into '() (take 0 (repeat 3)))", env))
        assertEquals(list(3L, 3L, 3L, 3L, 3L), eval("(into '() (take 5 (repeat 3)))", env))
        assertEquals(9, eval("(count (rest (take 10 (repeat 5))))", env))
        try {
            eval("(+ (repeat 3))", env)
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testRepeatedly() {
        assertEquals(list<Any>(), eval("(into '() (take 0 (repeatedly (lambda () 1))))", env))
        assertEquals(list(3L, 3L, 3L, 3L, 3L), eval("(into '() (take 5 (repeatedly (lambda () 3))))", env))
        try {
            eval("(+ (repeatedly 3))", env)
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testIterate() {
        assertEquals(list<Any>(), eval("(into '() (take 0 (iterate inc 0)))", env))
        assertEquals(list(0L, 1L, 2L, 3L, 4L), eval("(into '() (take 5 (iterate inc 0)))", env))
        assertEquals(list(0L, 2L, 4L, 6L, 8L), eval("(into '() (take 5 (iterate (partial + 2) 0)))", env))
        try {
            eval("(+ (iterate inc 0))", env)
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testCycle() {
        assertEquals(MutableVector(), eval("(into [] (take 0 (cycle [1 2 3])))", env))
        assertEquals(0, eval("(count (take 10 (cycle [])))", env))
        assertEquals(MutableVector(arrayOf(0L, 1L, 2L, 0L, 1L)), eval("(into [] (take 5 (cycle '(0 1 2))))", env))
        try {
            eval("(+ (iterate inc 0))", env)
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testConcat() {
        assertEquals(list<Nothing>(), eval("(into '() (concat [] '() #{}))", env))
        assertEquals(MutableVector(arrayOf(1L, 2L, 3L, 4L)), eval("(into [] (concat [1 2] [3 4]))", env))
        assertEquals(list(0L, 5L, 5L, 5L, 5L), eval("(into '() (take 5 (concat [0] (repeat 5))))", env))
        try {
            eval("(+ (concat []))", env)
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testSome() {
        assertEquals(true, eval("(some even? '(1 2 3 4))", env))
        assertEquals(null, eval("(some even? '(1 3 5 7))", env))
        assertEquals(null, eval("(some true? [false false false])", env))
        assertEquals(true, eval("(some true? [false true false])", env))
        assertEquals("three", eval("(some {2 \"two\" 3 \"three\"} [nil 3 2])", env))
        assertEquals("nothing", eval("(some {nil \"nothing\" 2 \"two\" 3 \"three\"} [nil 3 2])", env))
        assertEquals("two", eval("(some {2 \"two\" 3 nil} [nil 3 2])", env))
        assertEquals(true, eval("(some (lambda (n) (= n 1/2)) (range -5 5 0.5))", env))
    }

    @Test
    fun testEvery() {
        assertEquals(true,  eval("(every? true?  '())", env))
        assertEquals(true,  eval("(every? false? '())", env))
        assertEquals(true,  eval("(every? {1 \"one\" 2 \"two\"} [1 2])", env))
        assertEquals(false, eval("(every? {1 \"one\" 2 \"two\"} [1 2 3])", env))
        assertEquals(true,  eval("(every? even? '(2 4 6))", env))
        assertEquals(false, eval("(every? even? '(1 2 3))", env))
        assertEquals(true,  eval("(every? #{1 2} [1 2])", env))
        assertEquals(false, eval("(every? #{1 2} [1 2 3])", env))
    }
}
