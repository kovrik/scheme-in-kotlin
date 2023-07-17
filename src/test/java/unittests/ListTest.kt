package unittests

import core.exceptions.ArityException
import core.scm.MutableVector
import core.scm.Symbol
import org.junit.Assert.*
import org.junit.Test

class ListTest : AbstractTest() {

    @Test
    fun testEvalList() {
        assertEquals(true, eval("(list? (list 1 2 3 4 5))"))
        assertEquals(listOf(1L, 2L, 3L), eval("(list 1 2 3)"))
    }

    @Test
    fun testEvalIsList() {
        assertEquals(true,  eval("(list? '())"))
        assertEquals(true,  eval("(list? '(1 2 3))"))
        assertEquals(false, eval("(list? #(1 2 3))"))
        assertEquals(false, eval("(list? (cons 1 2))"))
        assertEquals(false, eval("(list? 2)"))
        assertEquals(true,  eval("(list? (car '((1 2 3))))"))
        assertEquals(true,  eval("(list? (cdr '((1 2 3))))"))
        assertEquals(false, eval("(list? (car '((1 . 2))))"))
        assertEquals(false, eval("(list? (vector-ref #((1 2 3 . 4)) 0))"))
        assertEquals(false, eval("(list? (vector-ref #((1 . 2)) 0))"))
    }

    @Test
    fun testEvalEmpty() {
        assertEquals(true,  eval("(nil?   null)"))
        assertEquals(true,  eval("(nil?   nil)"))
        assertEquals(true,  eval("(null?  nil)"))
        assertEquals(true,  eval("(null?  null)"))
        assertEquals(false, eval("(null?  '())"))
        assertEquals(true,  eval("(empty? '())"))
        assertEquals(false, eval("(null?  '(1 2 3))"))
        assertEquals(false, eval("(empty? '(1 2 3))"))
        assertEquals(false, eval("(null?  1)"))
        assertEquals(false, eval("(empty? 1)"))
        assertEquals(true,  eval("(empty? (cdr '(1)))"))
        assertEquals(true,  eval("(void? (void 1 2))"))
        assertEquals(false, eval("(void? null)"))
        assertEquals(false, eval("(void? 1)"))
    }

    @Test
    fun testEvalListToVector() {
        assertEquals(MutableVector(arrayOf(1L, 2L, "test")), eval("(list->vector '(1 2 \"test\"))"))
        assertEquals(MutableVector(), eval("(list->vector '())"))
        try {
            eval("(list->vector #(1 2 3))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertTrue(e.message!!.startsWith("list->vector: type mismatch; (expected: List, given:"))
        }
    }

    @Test
    fun testCons() {
        assertEquals(true, eval("(equal? '(1 2)   (cons 1 (cons 2 '())))"))
        assertEquals(true, eval("(equal? '(1 2 3) (cons 1 (cons 2 (cons 3 '()))))"))
        assertEquals(true, eval("(equal? '(1 2 3) (cons 1 '(2 3))))"))
        assertEquals(true, eval("(equal? '(1)     (cons 1 '())))"))
        assertEquals(true, eval("(equal? (cons 1 2) (cons 1 2)))"))
        // check that we do not modify the original list/cons, but return new instead
        eval("(define conslist '())")
        eval("(cons 1 conslist)")
        assertEquals(true, eval("(equal? '() conslist))"))
        eval("(define conslist '(3))")
        eval("(cons 1 conslist)")
        assertEquals(true, eval("(equal? '(3) conslist))"))
        assertEquals(Pair(null, 1L), eval("(cons nil 1)"))
        assertEquals(Pair(null, null), eval("(cons nil nil)"))
        assertEquals(listOf(listOf(0L), 0L, 1L, 2L, 3L, 4L), eval("(cons '(0) (range 5))"))
    }

    @Test
    fun testIsPair() {
        assertEquals(false, eval("(pair? '())"))
        assertEquals(false, eval("(pair? 1)"))
        assertEquals(false, eval("(pair? #(1 2))"))
        assertEquals(true, eval("(pair? '(1))"))
        assertEquals(true, eval("(pair? '(1 2))"))
        assertEquals(true, eval("(pair? '(1 2 3))"))
        assertEquals(true, eval("(pair? (cons 1 2))"))
        assertEquals(true, eval("(pair? (cons 1 '()))"))
        assertEquals(true, eval("(pair? (cons 1 (cons 2 3))))"))
    }

    @Test
    fun testCar() {
        assertEquals(1L, eval("(car (cons 1 2))"))
        assertEquals("test", eval("(car (cons \"test\" 2))"))
        assertEquals(1L, eval("(car (cons 1 (cons 2 3)))"))
        assertEquals(1L, eval("(car '(1 2 3))"))
        assertEquals(1L, eval("(car '(1))"))
        assertEquals(1L, eval("(car (list 1))"))
        try {
            eval("(car '())")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("car: type mismatch; (expected: PairOrNonEmptyList, given: ())", e.message)
        }
        try {
            eval("(car 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("car: type mismatch; (expected: PairOrNonEmptyList, given: 1)", e.message)
        }
    }

    @Test
    fun testCdr() {
        assertEquals(2L, eval("(cdr (cons 1 2))"))
        assertEquals("test", eval("(cdr (cons 2 \"test\"))"))
        assertEquals(Pair(2L, 3L), eval("(cdr (cons 1 (cons 2 3)))"))
        assertEquals(listOf(2L, 3L), eval("(cdr '(1 2 3))"))
        assertEquals(emptyList<Nothing>(), eval("(cdr '(1))"))
        assertEquals(emptyList<Nothing>(), eval("(cdr (list 1))"))
        try {
            eval("(cdr '())")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("cdr: type mismatch; (expected: PairOrNonEmptyList, given: ())", e.message)
        }
        try {
            eval("(cdr 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("cdr: type mismatch; (expected: PairOrNonEmptyList, given: 1)", e.message)
        }
    }

    @Test
    fun testSetMcar() {
        assertEquals(Unit,     eval("(set-mcar! (mcons 1 '()) 2)"))
        assertEquals(3L,       eval("(let ((a (mcons 1 '()))) (set-mcar! a 3) (mcar a))"))
        assertEquals("test",   eval("(let ((a (mcons 1 '(2 3)))) (set-mcar! a \"test\") (mcar a))"))
        assertEquals("test",   eval("(let ((a (mcons 3 4))) (set-mcar! a \"test\") (mcar a))"))
        try {
            eval("(set-mcar! '() 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("set-mcar!: type mismatch; (expected: MutablePair, given: ())", e.message)
        }
        try {
            eval("(set-mcar! 5 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("set-mcar!: type mismatch; (expected: MutablePair, given: 5)", e.message)
        }
    }

    @Test
    fun testSetMcdr() {
        assertEquals(3L,                 eval("(let ((a (mcons 1 '()))) (set-mcdr! a 3) (mcdr a))"))
        assertEquals("test",             eval("(let ((a (mcons 1 '()))) (set-mcdr! a \"test\") (mcdr a))"))
        assertEquals(listOf(2L, 3L, 4L), eval("(let ((a (mcons 1 '()))) (set-mcdr! a '(2 3 4)) (mcdr a))"))
        assertEquals(3L, eval("(let ((a (mcons 1 2))) (set-mcdr! a 3) (mcdr a))"))
        assertEquals(2L, eval("(let ((a (mcons 1 2))) (set-mcdr! a '(3 4 5)) (mcdr (mcons 1 2))"))
        try {
            eval("(set-mcdr! '() 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("set-mcdr!: type mismatch; (expected: MutablePair, given: ())", e.message)
        }
        try {
            eval("(set-mcdr! 5 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("set-mcdr!: type mismatch; (expected: MutablePair, given: 5)", e.message)
        }
    }

    @Test
    fun testAppend() {
        assertEquals("test", eval("(append '() \"test\")"))
        assertEquals(5L, eval("(append '() 5)"))
        assertEquals(Pair(1L, 5L), eval("(append '(1) 5)"))
        assertEquals(listOf(1L, 2L, 3L), eval("(append '(1) '(2 3))"))
        assertEquals(listOf(1L, 2L, 2L, 3L), eval("(append '(1 2) '(2 3))"))
        assertEquals(listOf(1L, 2L, 3L, 4L, 5L), eval("(append '(1) '(2) '(3 4) '(5))"))
        assertEquals(Pair(1L, 2L), eval("(append '() (cons 1 2))"))
        assertEquals(Pair(1L, Pair(1L, 2L)), eval("(append '(1) (cons 1 2))"))
        assertEquals(Pair(1L, Pair(1L, Pair(1L, 2L))), eval("(append '(1 1) (cons 1 2))"))
        assertEquals(emptyList<Nothing>(), eval("(append '() '() '() '())"))
        try {
            eval("(append 1 '())")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("append: type mismatch; (expected: List, given: 1)", e.message)
        }
        try {
            eval("(append '() '() 5 '())")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("append: type mismatch; (expected: List, given: 5)", e.message)
        }
    }

    @Test
    fun testReverse() {
        assertEquals(emptyList<Nothing>(), eval("(reverse '())"))
        assertEquals(listOf(1L), eval("(reverse '(1))"))
        assertEquals(listOf(1L, 2L, 3L), eval("(reverse '(3 2 1))"))
        assertEquals(listOf(1L, 2L, 3L), eval("(reverse (reverse '(1 2 3)))"))
        try {
            eval("(reverse 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            // expected
        }
        try {
            eval("(reverse '(1 2) '(3 4))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("reverse: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 2)", e.message)
        }
    }

    @Test
    fun testListTail() {
        assertEquals(listOf(3L, 4L), (eval("(list-tail (list 1 2 3 4) 2)") as Sequence<*>).toList())
        assertEquals(2L, eval("(list-tail (cons 1 2) 1)"))
        assertEquals(Symbol.intern("not-a-pair"), eval("(list-tail 'not-a-pair 0)"))

        eval("(define a '(1 2 3 4))")
        eval("(define b (list-tail (cdr a) 2))")
        assertEquals(listOf(4L), (eval("b") as Sequence<*>).toList())
        try {
            eval("(list-tail 1 2)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("list-tail: type mismatch; (expected: List, given: 1)", e.message)
        }
    }

    @Test
    fun testListRef() {
        assertEquals(1L, eval("(list-ref '(1) 0)"))
        assertEquals(3L, eval("(list-ref '(1 2 3) 2)"))
        assertEquals(1L, eval("(list-ref (cons 1 2) 0)"))
        assertEquals(Symbol.intern("c"), eval("(list-ref (list 'a 'b 'c) 2)"))
        assertEquals(Pair(1L, 2L), eval("(list-ref (list 1 2 (cons 1 2)) 2)"))
        assertEquals(listOf(1L, 2L), eval("(list-ref '(1 2 (1 2)) 2)"))
        try {
            eval("(list-ref 1 2)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("list-ref: type mismatch; (expected: PairOrNonEmptyList, given: 1)", e.message)
        }
        try {
            eval("(list-ref '(1 2) 2.5)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("list-ref: type mismatch; (expected: ExactNonNegativeInteger, given: 2.5)", e.message)
        }
    }

    @Test
    fun testListToString() {
        assertEquals("", eval("(list->string '())"))
        assertEquals("AB", eval("(list->string '(#\\A #\\B))"))
        assertEquals("B", eval("(list->string (cdr '(#\\A #\\B)))"))
        try {
            eval("(list->string (cons 1 2))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("list->string: type mismatch; (expected: List, given: (1 . 2))", e.message)
        }
        try {
            eval("(list->string (list 1 2))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("list->string: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalLength() {
        assertEquals(0, eval("(length '())"))
        assertEquals(1, eval("(length '(1))"))
        assertEquals(5, eval("(length '(1 2 3 4 5))"))
        try {
            eval("(length)")
            fail()
        } catch (e: ArityException) {
            assertEquals("length: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 0)", e.message)
        }
        try {
            eval("(length 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            // expected
        }
    }

    @Test
    fun testEvalMember() {
        assertEquals(false, eval("(member 0 '())"))
        assertEquals(false, eval("(member 0 '(1 2 3))"))
        assertEquals(false, eval("(member \"test\" '(1 2 3))"))

        assertEquals(listOf(1L, 2L, 3L), (eval("(member 1 '(1 2 3))") as Sequence<*>).toList())
        assertEquals(listOf(2L, 3L), (eval("(member 2 '(1 2 3))") as Sequence<*>).toList())
        assertEquals(listOf(3L), (eval("(member 3 '(1 2 3))") as Sequence<*>).toList())
        assertEquals(listOf(listOf(Symbol.intern("a")), Symbol.intern("c")), (eval("(member (list 'a) '(b (a) c))") as Sequence<*>).toList())
        try {
            eval("(member)")
            fail()
        } catch (e: ArityException) {
            assertEquals("member: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.message)
        }
        assertEquals(false, eval("(member 1 #())"))
    }

    @Test
    fun testEvalMemq() {
        assertEquals(false, eval("(memq 0 '())"))
        assertEquals(false, eval("(memq 0 '(1 2 3))"))
        assertEquals(false, eval("(memq \"test\" '(1 2 3))"))

        assertEquals(listOf(1L, 2L, 3L), (eval("(memq 1 '(1 2 3))") as Sequence<*>).toList())
        assertEquals(listOf(2L, 3L), (eval("(memq 2 '(1 2 3))") as Sequence<*>).toList())
        assertEquals(listOf(3L), (eval("(memq 3 '(1 2 3))") as Sequence<*>).toList())
        assertEquals(false, eval("(memq (list 'a) '(b (a) c))"))

        assertEquals(listOf(Symbol.intern("a"), Symbol.intern("b"), Symbol.intern("c")), (eval("(memq 'a '(a b c))") as Sequence<*>).toList())
        assertEquals(listOf(Symbol.intern("b"), Symbol.intern("c")), (eval("(memq 'b '(a b c))") as Sequence<*>).toList())
        assertEquals(false, eval("(memq 'a '(b c d))"))
        try {
            eval("(memq)")
            fail()
        } catch (e: ArityException) {
            assertEquals("memq: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.message)
        }
        assertEquals(false, eval("(memq 1 #())"))
    }

    @Test
    fun testEvalMemv() {
        assertEquals(false, eval("(memv 0 '())"))
        assertEquals(false, eval("(memv 0 '(1 2 3))"))
        assertEquals(false, eval("(memv \"test\" '(1 2 3))"))

        assertEquals(listOf(1L, 2L, 3L), (eval("(memv 1 '(1 2 3))") as Sequence<*>).toList())
        assertEquals(listOf(2L, 3L), (eval("(memv 2 '(1 2 3))") as Sequence<*>).toList())
        assertEquals(listOf(3L), (eval("(memv 3 '(1 2 3))") as Sequence<*>).toList())
        assertEquals(false, eval("(memv (list 'a) '(b (a) c))"))

        assertEquals(listOf(Symbol.intern("a"), Symbol.intern("b"), Symbol.intern("c")), (eval("(memv 'a '(a b c))") as Sequence<*>).toList())
        assertEquals(listOf(Symbol.intern("b"), Symbol.intern("c")), (eval("(memv 'b '(a b c))") as Sequence<*>).toList())
        assertEquals(false, eval("(memv 'a '(b c d))"))

        assertEquals(listOf(101L, 102L), (eval("(memv 101 '(100 101 102))") as Sequence<*>).toList())
        try {
            eval("(memv)")
            fail()
        } catch (e: ArityException) {
            assertEquals("memv: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.message)
        }
        assertEquals(false, eval("(memv 1 #())"))
    }

    @Test
    fun testEvalAssoc() {
        eval("(define e '((a 1) (b 2) (c 3)))")
        assertEquals(listOf(listOf(Symbol.intern("a")) as Any), eval("(assoc (list 'a) '(((a)) ((b)) ((c))))"))
        try {
            eval("(assoc)")
            fail()
        } catch (e: ArityException) {
            assertEquals("assoc: arity mismatch; the expected number of arguments does not match the given number (expected: at least 2, given: 0)", e.message)
        }
        try {
            eval("(assoc 1 #())")
            fail()
        } catch (e: IllegalArgumentException) {
            assertTrue(e.message!!.startsWith("assoc: type mismatch; (expected: List or Map, given:"))
        }
        try {
            eval("(assoc 1 '((a 2) 3))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("assoc: wrong type argument in position 1 (expecting association list): ((a 2) 3)", e.message)
        }
    }

    @Test
    fun testEvalAssq() {
        eval("(define e '((a 1) (b 2) (c 3)))")
        assertEquals(listOf(Symbol.intern("a"), 1L), eval("(assq 'a e)"))
        assertEquals(listOf(Symbol.intern("b"), 2L), eval("(assq 'b e)"))
        assertEquals(false, eval("(assq 'd e)"))
        try {
            eval("(assq)")
            fail()
        } catch (e: ArityException) {
            assertEquals("assq: arity mismatch; the expected number of arguments does not match the given number (expected: at least 2, given: 0)", e.message)
        }
        try {
            eval("(assq 1 #())")
            fail()
        } catch (e: IllegalArgumentException) {
            assertTrue(e.message!!.startsWith("assq: type mismatch; (expected: List, given:"))
        }
        try {
            eval("(assq 1 '((a 2) 3))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("assq: wrong type argument in position 1 (expecting association list): ((a 2) 3)", e.message)
        }
    }

    @Test
    fun testEvalAssv() {
        assertEquals(listOf(5L, 7L), eval("(assv 5 '((2 3) (5 7) (11 13)))"))
        try {
            eval("(assv)")
            fail()
        } catch (e: ArityException) {
            assertEquals("assv: arity mismatch; the expected number of arguments does not match the given number (expected: at least 2, given: 0)", e.message)
        }
        try {
            eval("(assv 1 #())")
            fail()
        } catch (e: IllegalArgumentException) {
            assertTrue(e.message!!.startsWith("assv: type mismatch; (expected: List, given:"))
        }
        try {
            eval("(assv 1 '((a 2) 3))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("assv: wrong type argument in position 1 (expecting association list): ((a 2) 3)", e.message)
        }
    }

    @Test
    fun testEvalFirst() {
        assertEquals(null, eval("(first '())"))
        assertEquals(1L, eval("(first '(1))"))
        assertEquals(2L, eval("(first '(2 3 4))"))
        assertEquals(emptyList<Nothing>(), eval("(first '(() 3 4))"))
        assertEquals(null, eval("(first [])"))
        assertEquals(1L, eval("(first [1])"))
        assertEquals(2L, eval("(first [2 3 4])"))
        assertEquals(emptyList<Nothing>(), eval("(first ['() 3 4])"))
        assertEquals(null, eval("(first #{})"))
        assertEquals(1L, eval("(first #{1})"))
        assertEquals(null, eval("(first \"\")"))
        assertEquals('t', eval("(first \"test\")"))
    }

    @Test
    fun testEvalSecond() {
        assertEquals(null, eval("(second '())"))
        assertEquals(null, eval("(second '(1))"))
        assertEquals(3L, eval("(second '(2 3 4))"))
        assertEquals(emptyList<Nothing>(), eval("(second '(3 () 3 4))"))
        assertEquals(null, eval("(second [])"))
        assertEquals(null, eval("(second [1])"))
        assertEquals(3L, eval("(second [2 3 4])"))
        assertEquals(emptyList<Nothing>(), eval("(second [3 '() 3 4])"))
        assertEquals(null, eval("(second \"\")"))
        assertEquals('e', eval("(second \"test\")"))
    }

    @Test
    fun testEvalNext() {
        assertEquals(null, eval("(next nil)"))
        assertEquals(null, eval("(next '())"))
        assertEquals(null, eval("(next '(1))"))
        assertEquals(null, eval("(next (seq '()))"))
        assertEquals(null, eval("(next (seq '(1)))"))
        assertEquals(1,    eval("(count (take 1 (next (repeat 5))))"))
        assertEquals(listOf(3L, 4L), (eval("(next '(2 3 4))") as Sequence<*>).toList())
        assertEquals(listOf(3L, 4L, 5L), (eval("(next (next (seq '(1 2 3 4 5))))") as Sequence<*>).toList())
    }

    @Test
    fun testEvalRest() {
        assertEquals(0, eval("(count (rest nil))"))
        assertEquals(0, eval("(count (rest '()))"))
        assertEquals(0, eval("(count (rest '(1)))"))
        assertEquals(listOf(3L, 4L), (eval("(rest '(2 3 4))") as Sequence<*>).toList())
    }

    @Test
    fun testEvalSort() {
        assertEquals(listOf(1L, 2L, 3L, 4L, 5L), (eval("(sort   '(5 4 3 2 1))") as Sequence<*>).toList())
        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), (eval("(sort > '(5 4 3 2 1))") as Sequence<*>).toList())
        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), (eval("(sort > '(5 4 3 2 1))") as Sequence<*>).toList())
        assertEquals(emptyList<Any?>(), (eval("(sort '())") as Sequence<*>).toList())
        assertEquals(listOf(1L, 2L, 3L, 4L, 5L), (eval("(sort   [5 4 3 2 1])") as Sequence<*>).toList())
        assertEquals(listOf(5L, 4L, 3L, 2L, 1L), (eval("(sort > [5 4 3 2 1])") as Sequence<*>).toList())
        assertEquals(emptyList<Any?>(), (eval("(sort [])") as Sequence<*>).toList())
        assertEquals(listOf(6L, 6L, 6L, 6L, 6L), eval("(into '() (let ((l '(1 2 3 4 5))) (map + l (sort > l))))"))
        assertEquals(listOf(6L, 6L, 6L, 6L, 6L), eval("(into '() (let ((l  [1 2 3 4 5])) (map + l (sort > l))))"))
        try {
            (eval("(sort > [1 2 \"test\" 'a])") as Sequence<*>).toList()
            fail()
        } catch (e: java.lang.ClassCastException) {
            // expected
        }
    }

    @Test
    fun testEvalTake() {
        assertEquals(0, eval("(count (take -3 '(1 2 3 4 5)))"))
        assertEquals(0, eval("(count (take  0 '(1 2 3 4 5)))"))
        assertEquals(0, eval("(count (take -3  [1 2 3 4 5]))"))
        assertEquals(0, eval("(count (take  0  [1 2 3 4 5]))"))
        assertEquals(0, eval("(count (take -3 #(1 2 3 4 5)))"))
        assertEquals(0, eval("(count (take  0 #(1 2 3 4 5)))"))
        assertEquals(0, eval("(count (take -3  {1 2 3 4}))"))
        assertEquals(0, eval("(count (take  0  {1 2 3 4}))"))
        assertEquals(listOf<Any?>(1L), (eval("(take  1 '(1 2 3 4 5))") as Sequence<*>).toList())
        assertEquals(listOf<Any?>(1L), (eval("(take  1  [1 2 3 4 5])") as Sequence<*>).toList())
        assertEquals(listOf<Any?>(1L), (eval("(take  1 #(1 2 3 4 5))") as Sequence<*>).toList())
        assertEquals(listOf<Any?>(1L, 2L, 3L), (eval("(take  3 '(1 2 3 4 5))") as Sequence<*>).toList())
        assertEquals(listOf<Any?>(1L, 2L, 3L), (eval("(take  3  [1 2 3 4 5])") as Sequence<*>).toList())
        assertEquals(listOf<Any?>(1L, 2L, 3L), (eval("(take  3 #(1 2 3 4 5))") as Sequence<*>).toList())
        assertEquals(listOf<Any?>(1L, 2L, 3L, 4L, 5L), (eval("(take  30 '(1 2 3 4 5))") as Sequence<*>).toList())
        assertEquals(listOf<Any?>(1L, 2L, 3L, 4L, 5L), (eval("(take  30  [1 2 3 4 5])") as Sequence<*>).toList())
        assertEquals(listOf<Any?>(1L, 2L, 3L, 4L, 5L), (eval("(take  30 #(1 2 3 4 5))") as Sequence<*>).toList())
    }
}
