package unittests

import core.exceptions.WrongTypeException
import core.scm.BigRatio
import core.scm.Cons.Companion.list
import core.scm.Keyword
import core.scm.MutableVector
import org.junit.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class SeqTest : AbstractTest() {

    @Test
    fun testRepeat() {
        assertEquals(list<Any?>(null, null, null), eval("(into '() (take 3 (repeat nil)))", env))
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

        eval("(def powers-of-two (iterate (partial * 2) 1))", env)
        assertEquals(1024L, eval("(nth powers-of-two 10)", env))
        assertEquals(listOf(1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L, 256L, 512L),
                     eval("(into '() (take 10 powers-of-two))", env))
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
        assertEquals(listOf(null, null, null, null, null), eval("(into '() (take 5 (cycle [nil nil])))", env))
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

    @Test
    fun testLast() {
        assertEquals(null, eval("(last  [])", env))
        assertEquals(null, eval("(last '())", env))
        assertEquals(1L,   eval("(last  [1])", env))
        assertEquals(1L,   eval("(last '(1))", env))
        assertEquals(1L,   eval("(last  [5 4 3 2 1])", env))
        assertEquals(1L,   eval("(last '(5 4 3 2 1))", env))
        assertEquals(14L,  eval("(last (range 0 16 2))", env))
        assertEquals(3L,   eval("(last (take 5 (repeat 3)))", env))
        assertEquals(5L, eval("(last (take 5 (map inc (range))))", env))
    }

    @Test
    fun testButlast() {
        assertEquals(list(1L, 2L, 3L, 4L), eval("(into '() (butlast (map (fn (n) (+ n 1)) (range 5))))", env))
        assertEquals(list(1L, 2L), eval("(into '() (butlast [1 2 3]))", env))
        assertEquals(list(1L),     eval("(into '() (butlast (butlast [1 2 3])))", env))
        assertEquals(null,         eval("(butlast (butlast (butlast [1 2 3])))", env))
        assertEquals(null,         eval("(butlast (butlast (butlast (butlast [1 2 3]))))", env))
        assertEquals(null,         eval("(butlast nil)", env))
    }

    @Test
    fun testRange() {
        assertEquals(emptyList<Nothing>(),       eval("(into '() (take 0 (range)))", env))
        assertEquals(listOf(0L, 1L, 2L, 3L),     eval("(into '() (take 4 (range)))", env))
        assertEquals(listOf(-3L, -2L, -1L, 0L, 1L, 2L), eval("(into '() (range -3 3))", env))
        assertEquals(listOf(0L, 1L, 2L, 3L, 4L), eval("(into '() (range 5))", env))
        assertEquals(listOf(0L, -1L, -2L, -3L, -4L), eval("(into '() (range 0 -5 -1))", env))
        assertEquals(listOf(2L, 3L, 4L),         eval("(into '() (range 2 5))", env))
        assertEquals(listOf(0L, 2L, 4L, 6L),     eval("(into '() (range 0 8 2))", env))
        assertEquals(listOf(0.0, 2.5),           eval("(into '() (range 0 5 2.5))", env))
        assertEquals(listOf(0.0, 1.0, 2.0, 3.0), eval("(into '() (range 0 4 1.0))", env))
        assertEquals(listOf(0.0, 0.3, 0.6, 0.8999999999999999), eval("(into '() (range 0 1 0.3))", env))
        assertEquals(listOf(0L, BigRatio.valueOf("1", "2"), BigRatio.ONE, BigRatio.valueOf("3", "2")),
                     eval("(into '() (range 0 2 1/2))", env))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 0))", env))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 0.0))", env))
        assertEquals(listOf<Nothing>(), eval("(into '() (range (bigint 0)))", env))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 0/1))", env))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 5 5))", env))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 5.0 5.0))", env))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 5 5 1/2))", env))
        assertEquals(listOf<Nothing>(), eval("(into '() (range (bigint 5) (bigint 5)))", env))
    }

    @Test
    fun testFlatten() {
        assertEquals(listOf<Nothing>(), eval("(flatten (range 0))", env))
        assertEquals(listOf<Nothing>(), eval("(flatten '())", env))
        assertEquals(listOf<Nothing>(), eval("(flatten [])", env))
        assertEquals(listOf<Nothing>(), eval("(flatten #{})", env))
        assertEquals(listOf(0L, 1L, 2L, 3L, 4L), eval("(flatten '(0 (1) ((2)) (((3))) ((((4))))))", env))
        // FIXME
//        assertEquals(listOf(0L, 1L, 2L, 3L, 4L), eval("(flatten (list 0 [1] #{ [2] } (range 3 4) '((((4))))))", env))
    }

    @Test
    fun testIsColl() {
        assertEquals(true,  eval("(coll?  {})", env))
        assertEquals(true,  eval("(coll? #{})", env))
        assertEquals(true,  eval("(coll?  [])", env))
        assertEquals(true,  eval("(coll? '())", env))
        assertEquals(true,  eval("(coll? (range))", env))
        assertEquals(true,  eval("(coll? (seq \"test\"))", env))
        assertEquals(false, eval("(coll? \"test\")", env))
        assertEquals(false, eval("(coll? 5)",   env))
        assertEquals(false, eval("(coll? nil)", env))
    }

    @Test
    fun testFilter() {
        assertEquals(listOf(0L, 2L, 4L, 6L, 8L), eval("(filter even? (range 10))", env))
        assertEquals(listOf(2L, 3L), eval("(filter #{0 1 2 3} #{2 3 4 5})", env))
        assertEquals(listOf(1L, MutableVector(), Keyword.intern("a")), eval("(filter some? '(1 nil [] :a nil))", env))
    }

    @Test
    fun testDropLast() {
        assertEquals(emptyList<Nothing>(),   eval("(into '() (drop-last nil))", env))
        assertEquals(emptyList<Nothing>(),   eval("(into '() (drop-last  []))", env))
        assertEquals(emptyList<Nothing>(),   eval("(into '() (drop-last '()))", env))
        assertEquals(emptyList<Nothing>(),   eval("(into '() (drop-last  [1]))", env))
        assertEquals(emptyList<Nothing>(),   eval("(into '() (drop-last '(1)))", env))
        assertEquals(listOf(5L, 4L, 3L, 2L), eval("(into '() (drop-last  [5 4 3 2 1]))", env))
        assertEquals(listOf(5L, 4L, 3L, 2L), eval("(into '() (drop-last '(5 4 3 2 1)))", env))
        assertEquals(listOf(0L, 3L, 6L, 9L, 12L), eval("(into '() (drop-last (range 0 16 3)))", env))
        assertEquals(listOf(3L, 3L, 3L, 3L), eval("(into '() (drop-last (take 5 (repeat 3))))", env))
        assertEquals(listOf(1L, 2L, 3L, 4L), eval("(into '() (drop-last (take 5 (map inc (range)))))", env))

        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 3  []))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 3 '()))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 3  [1]))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 3 '(1)))", env))
        assertEquals(listOf(5L, 4L),       eval("(into '() (drop-last 3  [5 4 3 2 1]))", env))
        assertEquals(listOf(5L, 4L),       eval("(into '() (drop-last 3 '(5 4 3 2 1)))", env))
        assertEquals(listOf(0L, 3L, 6L),   eval("(into '() (drop-last 3 (range 0 16 3)))", env))
        assertEquals(listOf(3L, 3L),       eval("(into '() (drop-last 3 (take 5 (repeat 3))))", env))
        assertEquals(listOf(1L, 2L),       eval("(into '() (drop-last 3 (take 5 (map inc (range)))))", env))

        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30  []))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 '()))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30  [1]))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 '(1)))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30  [5 4 3 2 1]))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 '(5 4 3 2 1)))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 (range 0 16 3)))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 (take 5 (repeat 3))))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 (take 5 (map inc (range)))))", env))

        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), eval("(into '() (drop-last 0 '(5 4 3 2 1)))", env))
        assertEquals(listOf(0L, 3L, 6L, 9L, 12L, 15L), eval("(into '() (drop-last 0 (range 0 16 3)))", env))

        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), eval("(into '() (drop-last -10 '(5 4 3 2 1)))", env))
        assertEquals(listOf(0L, 3L, 6L, 9L, 12L, 15L), eval("(into '() (drop-last -10 (range 0 16 3)))", env))
    }

    @Test
    fun testTakeLast() {
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last nil))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last  []))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last '()))", env))
        assertEquals(listOf(1L),  eval("(into '() (take-last  [1]))", env))
        assertEquals(listOf(1L),  eval("(into '() (take-last '(1)))", env))
        assertEquals(listOf(1L),  eval("(into '() (take-last  [5 4 3 2 1]))", env))
        assertEquals(listOf(1L),  eval("(into '() (take-last '(5 4 3 2 1)))", env))
        assertEquals(listOf(15L), eval("(into '() (take-last (range 0 16 3)))", env))
        assertEquals(listOf(3L),  eval("(into '() (take-last (take 5 (repeat 3))))", env))
        assertEquals(listOf(5L),  eval("(into '() (take-last (take 5 (map inc (range)))))", env))

        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 3  []))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 3 '()))", env))
        assertEquals(listOf(1L),           eval("(into '() (take-last 3  [1]))", env))
        assertEquals(listOf(1L),           eval("(into '() (take-last 3 '(1)))", env))
        assertEquals(listOf(3L, 2L, 1L),   eval("(into '() (take-last 3  [5 4 3 2 1]))", env))
        assertEquals(listOf(3L, 2L, 1L),   eval("(into '() (take-last 3 '(5 4 3 2 1)))", env))
        assertEquals(listOf(9L, 12L, 15L), eval("(into '() (take-last 3 (range 0 16 3)))", env))
        assertEquals(listOf(3L, 3L, 3L),   eval("(into '() (take-last 3 (take 5 (repeat 3))))", env))
        assertEquals(listOf(3L, 4L, 5L),   eval("(into '() (take-last 3 (take 5 (map inc (range)))))", env))

        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 30  []))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 30 '()))", env))
        assertEquals(listOf(1L), eval("(into '() (take-last 30  [1]))", env))
        assertEquals(listOf(1L), eval("(into '() (take-last 30 '(1)))", env))
        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), eval("(into '() (take-last 30  [5 4 3 2 1]))", env))
        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), eval("(into '() (take-last 30 '(5 4 3 2 1)))", env))
        assertEquals(listOf(0L, 3L, 6L, 9L, 12L, 15L), eval("(into '() (take-last 30 (range 0 16 3)))", env))
        assertEquals(listOf(3L, 3L, 3L, 3L, 3L), eval("(into '() (take-last 30 (take 5 (repeat 3))))", env))
        assertEquals(listOf(1L, 2L, 3L, 4L, 5L), eval("(into '() (take-last 30 (take 5 (map inc (range)))))", env))

        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 0 '(5 4 3 2 1)))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 0 (range 0 16 3)))", env))

        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last -10 '(5 4 3 2 1)))", env))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last -10 (range 0 16 3)))", env))
    }
}
