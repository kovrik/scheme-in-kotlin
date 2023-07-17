package unittests

import core.exceptions.WrongTypeException
import core.scm.Ratio
import core.scm.Keyword
import core.scm.MutableVector
import org.junit.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class SeqTest : AbstractTest() {

    @Test
    fun testRepeat() {
        assertEquals(listOf<Any?>(null, null, null), eval("(into '() (take 3 (repeat nil)))"))
        assertEquals(listOf<Any>(), eval("(into '() (take 0 (repeat 3)))"))
        assertEquals(listOf(3L, 3L, 3L, 3L, 3L), eval("(into '() (take 5 (repeat 3)))"))
        assertEquals(9, eval("(count (rest (take 10 (repeat 5))))"))
        try {
            eval("(+ (repeat 3))")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testRepeatedly() {
        assertEquals(listOf<Any>(), eval("(into '() (take 0 (repeatedly (lambda () 1))))"))
        assertEquals(listOf(3L, 3L, 3L, 3L, 3L), eval("(into '() (take 5 (repeatedly (lambda () 3))))"))
        try {
            eval("(+ (repeatedly 3))")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testIterate() {
        assertEquals(listOf<Any>(), eval("(into '() (take 0 (iterate inc 0)))"))
        assertEquals(listOf(0L, 1L, 2L, 3L, 4L), eval("(into '() (take 5 (iterate inc 0)))"))
        assertEquals(listOf(0L, 2L, 4L, 6L, 8L), eval("(into '() (take 5 (iterate (partial + 2) 0)))"))

        eval("(def powers-of-two (iterate (partial * 2) 1))")
        assertEquals(1024L, eval("(nth powers-of-two 10)"))
        assertEquals(
            listOf(1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L, 256L, 512L),
            eval("(into '() (take 10 powers-of-two))")
        )
        try {
            eval("(+ (iterate inc 0))")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testCycle() {
        assertEquals(MutableVector(), eval("(into [] (take 0 (cycle [1 2 3])))"))
        assertEquals(0, eval("(count (take 10 (cycle [])))"))
        assertEquals(MutableVector(arrayOf(0L, 1L, 2L, 0L, 1L)), eval("(into [] (take 5 (cycle '(0 1 2))))"))
        assertEquals(listOf(null, null, null, null, null), eval("(into '() (take 5 (cycle [nil nil])))"))
        try {
            eval("(+ (iterate inc 0))")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testConcat() {
        assertEquals(listOf<Nothing>(), eval("(into '() (concat [] '() #{}))"))
        assertEquals(MutableVector(arrayOf(1L, 2L, 3L, 4L)), eval("(into [] (concat [1 2] [3 4]))"))
        assertEquals(listOf(0L, 5L, 5L, 5L, 5L), eval("(into '() (take 5 (concat [0] (repeat 5))))"))
        try {
            eval("(+ (concat []))")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testSome() {
        assertEquals(true, eval("(some even? '(1 2 3 4))"))
        assertEquals(null, eval("(some even? '(1 3 5 7))"))
        assertEquals(null, eval("(some true? [false false false])"))
        assertEquals(true, eval("(some true? [false true false])"))
        assertEquals("three", eval("(some {2 \"two\" 3 \"three\"} [nil 3 2])"))
        assertEquals("nothing", eval("(some {nil \"nothing\" 2 \"two\" 3 \"three\"} [nil 3 2])"))
        assertEquals("two", eval("(some {2 \"two\" 3 nil} [nil 3 2])"))
        assertEquals(true, eval("(some (lambda (n) (= n 1/2)) (range -5 5 0.5))"))
    }

    @Test
    fun testEvery() {
        assertEquals(true, eval("(every? true?  '())"))
        assertEquals(true, eval("(every? false? '())"))
        assertEquals(true, eval("(every? {1 \"one\" 2 \"two\"} [1 2])"))
        assertEquals(false, eval("(every? {1 \"one\" 2 \"two\"} [1 2 3])"))
        assertEquals(true, eval("(every? even? '(2 4 6))"))
        assertEquals(false, eval("(every? even? '(1 2 3))"))
        assertEquals(true, eval("(every? #{1 2} [1 2])"))
        assertEquals(false, eval("(every? #{1 2} [1 2 3])"))
    }

    @Test
    fun testLast() {
        assertEquals(null, eval("(last  [])"))
        assertEquals(null, eval("(last '())"))
        assertEquals(1L, eval("(last  [1])"))
        assertEquals(1L, eval("(last '(1))"))
        assertEquals(1L, eval("(last  [5 4 3 2 1])"))
        assertEquals(1L, eval("(last '(5 4 3 2 1))"))
        assertEquals(14L, eval("(last (range 0 16 2))"))
        assertEquals(3L, eval("(last (take 5 (repeat 3)))"))
        assertEquals(5L, eval("(last (take 5 (map inc (range))))"))
    }

    @Test
    fun testButlast() {
        assertEquals(listOf(1L, 2L, 3L, 4L), eval("(into '() (butlast (map (fn (n) (+ n 1)) (range 5))))"))
        assertEquals(listOf(1L, 2L), eval("(into '() (butlast [1 2 3]))"))
        assertEquals(listOf(1L), eval("(into '() (butlast (butlast [1 2 3])))"))
        assertEquals(null, eval("(butlast (butlast (butlast [1 2 3])))"))
        assertEquals(null, eval("(butlast (butlast (butlast (butlast [1 2 3]))))"))
        assertEquals(null, eval("(butlast nil)"))
    }

    @Test
    fun testRange() {
        assertEquals(emptyList<Nothing>(), eval("(into '() (take 0 (range)))"))
        assertEquals(listOf(0L, 1L, 2L, 3L), eval("(into '() (take 4 (range)))"))
        assertEquals(listOf(-3L, -2L, -1L, 0L, 1L, 2L), eval("(into '() (range -3 3))"))
        assertEquals(listOf(0L, 1L, 2L, 3L, 4L), eval("(into '() (range 5))"))
        assertEquals(listOf(0L, -1L, -2L, -3L, -4L), eval("(into '() (range 0 -5 -1))"))
        assertEquals(listOf(2L, 3L, 4L), eval("(into '() (range 2 5))"))
        assertEquals(listOf(0L, 2L, 4L, 6L), eval("(into '() (range 0 8 2))"))
        assertEquals(listOf(0.0, 2.5), eval("(into '() (range 0 5 2.5))"))
        assertEquals(listOf(0.0, 1.0, 2.0, 3.0), eval("(into '() (range 0 4 1.0))"))
        assertEquals(listOf(0.0, 0.3, 0.6, 0.8999999999999999), eval("(into '() (range 0 1 0.3))"))
        assertEquals(
            listOf(0L, Ratio.valueOf("1", "2"), Ratio.ONE, Ratio.valueOf("3", "2")),
            eval("(into '() (range 0 2 1/2))")
        )
        assertEquals(listOf(1L, 2L), eval("(into '() (take 2 (range 1 (/ (expt 123 123) (expt 33 123)))))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 0))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 0.0))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (range (bigint 0)))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 0/1))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 5 5))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 5.0 5.0))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (range 5 5 1/2))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (range (bigint 5) (bigint 5)))"))
    }

    @Test
    fun testFlatten() {
        assertEquals(listOf<Nothing>(), eval("(into '() (flatten (range 0)))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (flatten '()))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (flatten []))"))
        assertEquals(listOf<Nothing>(), eval("(into '() (flatten #{}))"))
        assertEquals(listOf(0L, 1L, 2L, 3L, 4L), eval("(into '() (flatten '(0 (1) ((2)) (((3))) ((((4)))))))"))
        assertEquals(listOf(0L, 1L, 2L, 3L, 4L), eval("(into '() (flatten (list 0 [1] #{ [2] } (range 3 4) '((((4)))))))"))
    }

    @Test
    fun testIsColl() {
        assertEquals(true, eval("(coll?  {})"))
        assertEquals(true, eval("(coll? #{})"))
        assertEquals(true, eval("(coll?  [])"))
        assertEquals(true, eval("(coll? '())"))
        assertEquals(true, eval("(coll? (range))"))
        assertEquals(true, eval("(coll? (seq \"test\"))"))
        assertEquals(false, eval("(coll? \"test\")"))
        assertEquals(false, eval("(coll? 5)"))
        assertEquals(false, eval("(coll? nil)"))
    }

    @Test
    fun testFilter() {
        assertEquals(listOf(0L, 2L, 4L, 6L, 8L), eval("(into '() (filter even? (range 10)))"))
        assertEquals(listOf(0L, 2L, 4L, 6L, 8L), eval("(into '() (take 5 (filter even? (range))))"))
        assertEquals(listOf(2L, 3L), eval("(into '() (filter #{0 1 2 3} #{2 3 4 5}))"))
        assertEquals(
            listOf(1L, MutableVector(), Keyword.intern("a")),
            eval("(into '() (filter some? '(1 nil [] :a nil)))")
        )
    }

    @Test
    fun testDropLast() {
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last nil))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last  []))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last '()))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last  [1]))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last '(1)))"))
        assertEquals(listOf(5L, 4L, 3L, 2L), eval("(into '() (drop-last  [5 4 3 2 1]))"))
        assertEquals(listOf(5L, 4L, 3L, 2L), eval("(into '() (drop-last '(5 4 3 2 1)))"))
        assertEquals(listOf(0L, 3L, 6L, 9L, 12L), eval("(into '() (drop-last (range 0 16 3)))"))
        assertEquals(listOf(3L, 3L, 3L, 3L), eval("(into '() (drop-last (take 5 (repeat 3))))"))
        assertEquals(listOf(1L, 2L, 3L, 4L), eval("(into '() (drop-last (take 5 (map inc (range)))))"))

        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 3  []))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 3 '()))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 3  [1]))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 3 '(1)))"))
        assertEquals(listOf(5L, 4L), eval("(into '() (drop-last 3  [5 4 3 2 1]))"))
        assertEquals(listOf(5L, 4L), eval("(into '() (drop-last 3 '(5 4 3 2 1)))"))
        assertEquals(listOf(0L, 3L, 6L), eval("(into '() (drop-last 3 (range 0 16 3)))"))
        assertEquals(listOf(3L, 3L), eval("(into '() (drop-last 3 (take 5 (repeat 3))))"))
        assertEquals(listOf(1L, 2L), eval("(into '() (drop-last 3 (take 5 (map inc (range)))))"))

        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30  []))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 '()))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30  [1]))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 '(1)))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30  [5 4 3 2 1]))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 '(5 4 3 2 1)))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 (range 0 16 3)))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 (take 5 (repeat 3))))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (drop-last 30 (take 5 (map inc (range)))))"))

        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), eval("(into '() (drop-last 0 '(5 4 3 2 1)))"))
        assertEquals(listOf(0L, 3L, 6L, 9L, 12L, 15L), eval("(into '() (drop-last 0 (range 0 16 3)))"))

        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), eval("(into '() (drop-last -10 '(5 4 3 2 1)))"))
        assertEquals(listOf(0L, 3L, 6L, 9L, 12L, 15L), eval("(into '() (drop-last -10 (range 0 16 3)))"))
    }

    @Test
    fun testTakeLast() {
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last nil))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last  []))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last '()))"))
        assertEquals(listOf(1L), eval("(into '() (take-last  [1]))"))
        assertEquals(listOf(1L), eval("(into '() (take-last '(1)))"))
        assertEquals(listOf(1L), eval("(into '() (take-last  [5 4 3 2 1]))"))
        assertEquals(listOf(1L), eval("(into '() (take-last '(5 4 3 2 1)))"))
        assertEquals(listOf(15L), eval("(into '() (take-last (range 0 16 3)))"))
        assertEquals(listOf(3L), eval("(into '() (take-last (take 5 (repeat 3))))"))
        assertEquals(listOf(5L), eval("(into '() (take-last (take 5 (map inc (range)))))"))

        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 3  []))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 3 '()))"))
        assertEquals(listOf(1L), eval("(into '() (take-last 3  [1]))"))
        assertEquals(listOf(1L), eval("(into '() (take-last 3 '(1)))"))
        assertEquals(listOf(3L, 2L, 1L), eval("(into '() (take-last 3  [5 4 3 2 1]))"))
        assertEquals(listOf(3L, 2L, 1L), eval("(into '() (take-last 3 '(5 4 3 2 1)))"))
        assertEquals(listOf(9L, 12L, 15L), eval("(into '() (take-last 3 (range 0 16 3)))"))
        assertEquals(listOf(3L, 3L, 3L), eval("(into '() (take-last 3 (take 5 (repeat 3))))"))
        assertEquals(listOf(3L, 4L, 5L), eval("(into '() (take-last 3 (take 5 (map inc (range)))))"))

        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 30  []))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 30 '()))"))
        assertEquals(listOf(1L), eval("(into '() (take-last 30  [1]))"))
        assertEquals(listOf(1L), eval("(into '() (take-last 30 '(1)))"))
        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), eval("(into '() (take-last 30  [5 4 3 2 1]))"))
        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), eval("(into '() (take-last 30 '(5 4 3 2 1)))"))
        assertEquals(listOf(0L, 3L, 6L, 9L, 12L, 15L), eval("(into '() (take-last 30 (range 0 16 3)))"))
        assertEquals(listOf(3L, 3L, 3L, 3L, 3L), eval("(into '() (take-last 30 (take 5 (repeat 3))))"))
        assertEquals(listOf(1L, 2L, 3L, 4L, 5L), eval("(into '() (take-last 30 (take 5 (map inc (range)))))"))

        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 0 '(5 4 3 2 1)))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last 0 (range 0 16 3)))"))

        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last -10 '(5 4 3 2 1)))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (take-last -10 (range 0 16 3)))"))
    }

    @Test
    fun testLazySeq() {
        assertEquals(emptyList<Nothing>(), eval("(into '() (lazy-seq nil))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (lazy-seq '()))"))
        assertEquals(emptyList<Nothing>(), eval("(into '() (lazy-seq []))"))
        assertEquals(listOf(0L, 1L, 2L, 3L), eval("(into '() (take 4 (lazy-seq (range))))"))
        assertEquals(listOf(0L, 1L, 2L, 3L), eval("(into '() (take 4 (lazy-seq (range 5))))"))
        assertEquals(listOf(0L, 1L), eval("(into '() (take 4 (lazy-seq (range 2))))"))

        eval("(define (rfib a b) (lazy-seq (cons-seq a (rfib b (+ a b)))))\n")
        assertEquals(listOf(0L, 1L, 1L, 2L, 3L), eval("(into '() (take 5 (rfib 0 1)))"))

        assertEquals(
            1L, eval(
                "(let* ((b (atom 0))" +
                        "       (work (map (lambda (n) (swap! b inc)) [nil])))" +
                        "  (str work) (str work) (str work)" +
                        "  @b)")
        )

        assertEquals(
            3L, eval(
                "(let* ((b (atom 0))" +
                        "       (work (map (lambda (n) (swap! b inc)) [nil nil nil])))" +
                        "  (str work) (str work) (str work)" +
                        "  @b)")
        )
    }

    @Test
    fun testContains() {
        assertEquals(false, eval("(contains? []  1)"))
        assertEquals(false, eval("(contains? '() 1)"))
        assertEquals(false, eval("(contains? #() 1)"))
        assertEquals(false, eval("(contains? (cons 1 2) 3)"))
        assertEquals(true, eval("(contains? [1]  1)"))
        assertEquals(true, eval("(contains? '(1) 1)"))
        assertEquals(true, eval("(contains? #(1) 1)"))
        assertEquals(true, eval("(contains? (cons 1 2) 1)"))
        assertEquals(true, eval("(contains? [1 2 3 4 5]  4)"))
        assertEquals(true, eval("(contains? '(1 2 3 4 5) 4)"))
        assertEquals(true, eval("(contains? #(1 2 3 4 5) 4)"))
        assertEquals(false, eval("(contains? [1 2 3 4 5]  :a)"))
        assertEquals(false, eval("(contains? '(1 2 3 4 5) :a)"))
        assertEquals(false, eval("(contains? #(1 2 3 4 5) :a)"))
        assertEquals(true, eval("(contains? '(nil) nil)"))
        assertEquals(true, eval("(contains? '(()) '())"))
    }

    @Test
    fun testDistinct() {
        assertEquals(listOf<Any?>(), eval("(into '() (distinct '()))"))
        assertEquals(listOf(1L, 2L, 3L), eval("(into '() (distinct '(1 2 3)))"))
        assertEquals(listOf(1L, 2L, 3L), eval("(into '() (distinct '(1 1 2 2 3 3)))"))
        assertEquals(listOf(true, false), eval("(into '() (distinct '(#t #t #f #f)))"))
    }
}
