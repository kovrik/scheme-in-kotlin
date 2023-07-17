package unittests

import core.scm.MutableVector
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test

class VectorTest : AbstractTest() {

    @Test
    fun testEvalIsVector() {
        assertEquals(false, eval("(vector? #\\A)"))
        assertEquals(true,  eval("(vector? #(1 2 3 ))"))
    }

    @Test
    fun testEmptyVector() {
        assertEquals(true,  eval("(identical? #() #())"))
        assertEquals(true,  eval("(identical? [] [])"))
        assertEquals(false, eval("(identical? [] #())"))
    }

    @Test
    fun testEvalVector() {
        assertEquals(MutableVector(), eval("#()"))
        assertEquals(MutableVector(arrayOf(1L, 2L, 3L)), eval("#(1 2 3 )"))
        assertEquals(MutableVector(), eval("(vector)"))
        assertEquals(MutableVector(arrayOf(1L, 2L, 3L)), eval("(vector 1 2 3)"))
        assertEquals(MutableVector(arrayOf(1L, 2L, 3L)), eval("(vector 1 2 (+ 1 2))"))
        assertEquals(listOf(3L, 2L, 1L), eval("(reverse (vector 1 2 3))"))
        assertEquals(listOf(3L, 2L),     eval("(reverse (vector 2 3))"))
        assertEquals(listOf(3L),         eval("(reverse (vector 3))"))
        assertEquals(emptyList<Any?>(),  eval("(reverse (vector))"))
        assertEquals(7L, eval("([(+ 1 2) (+ 3 4)] 1)"))
    }

    @Test
    fun testEvalMakeVector() {
        assertEquals(MutableVector(arrayOf(1L, 1L, 1L)), eval("(make-vector 3 1)"))
        assertEquals(MutableVector(), eval("(make-vector 0)"))
        assertEquals(MutableVector(arrayOf(null, null, null)), eval("(make-vector 3)"))
        try {
            eval("(make-vector 1 2 3)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("make-vector: arity mismatch; the expected number of arguments does not match the given number (expected: 1 to 2, given: 3)", e.message)
        }

        try {
            eval("(make-vector \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("make-vector: type mismatch; (expected: ExactNonNegativeInteger, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalVectorLength() {
        assertEquals(0L, eval("(vector-length #())"))
        assertEquals(0L, eval("(vector-length (vector))"))
        assertEquals(3L, eval("(vector-length (vector 1 2 3))"))

        try {
            eval("(vector-length 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("vector-length: type mismatch; (expected: Vector, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalVectorRef() {
        assertEquals(1L, eval("(vector-ref (vector 1 2 3) 0)"))
        assertEquals(2L, eval("(vector-ref (vector 1 2 3) 1)"))
        assertEquals(3L, eval("(vector-ref (vector 1 2 3) 2)"))
        assertEquals("test", eval("(vector-ref (vector \"test\" 2 3) 0)"))
        try {
            eval("(vector-ref (vector 1 2 3) -1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("vector-ref: type mismatch; (expected: ExactNonNegativeInteger, given: -1)", e.message)
        }
        try {
            eval("(vector-ref (vector 1 2 3) 3)")
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("vector: value out of range: 3", e.message)
        }
        try {
            eval("(vector-ref (vector) 0)")
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("vector: value out of range: 0", e.message)
        }
        try {
            eval("(vector-ref '(1 2 3) 0)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("vector-ref: type mismatch; (expected: Vector, given: (1 2 3))", e.message)
        }
        try {
            eval("(vector-ref (vector 1 2 3) 0.5)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("vector-ref: type mismatch; (expected: ExactNonNegativeInteger, given: 0.5)", e.message)
        }
    }

    @Test
    fun testEvalVectorSet() {

        var sexp = "(begin (define v (vector 1 2 3))" +
                   "       (vector-set! v 0 99)" +
                   "       (vector-ref  v 0))"
        assertEquals(99L, eval(sexp))

        sexp = "(begin (define v (vector 1 2 3))" +
               "       (vector-set! v 2 \"test\")" +
               "       (vector-ref  v 2))"
        assertEquals("test", eval(sexp))

        sexp = "(begin (define v (vector 1 2 3)) (vector-set! v -1 \"test\"))"
        try {
            eval(sexp)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("vector-set!: type mismatch; (expected: ExactNonNegativeInteger, given: -1)", e.message)
        }

        sexp = "(begin (define v (vector 1 2 3)) (vector-set! v 3 \"test\"))"
        try {
            eval(sexp)
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("vector: value out of range: 3", e.message)
        }

        sexp = "(begin (define v (vector)) (vector-set! v 0 \"test\"))"
        try {
            eval(sexp)
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("vector: value out of range: 0", e.message)
        }

        sexp = "(begin (define v '(1 2 3)) (vector-set! v 0 \"test\"))"
        try {
            eval(sexp)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("vector-set!: type mismatch; (expected: MutableVector, given: (1 2 3))", e.message)
        }

        sexp = "(begin (define v (vector 1 2)) (vector-set! v 0.5 \"test\"))"
        try {
            eval(sexp)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("vector-set!: type mismatch; (expected: ExactNonNegativeInteger, given: 0.5)", e.message)
        }
    }

    @Test
    fun testEvalVectorToList() {

        assertEquals(listOf(1L, 2L, "test"), eval("(vector->list #(1 2 \"test\"))"))
        assertEquals(listOf<Any>(), eval("(vector->list #())"))

        try {
            eval("(vector->list '(1 2 3))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("vector->list: type mismatch; (expected: Vector, given: (1 2 3))", e.message)
        }
    }

    @Test
    fun testEvalVectorFill() {

        var sexp = "(begin (define v (vector 1 2 3))" +
                   "       (vector-fill! v 3)" +
                   "       v)"
        assertEquals(MutableVector(arrayOf(3L, 3L, 3L)), eval(sexp))

        sexp = "(begin (define v (vector))" +
               "       (vector-fill! v 3)" +
               "       v)"
        assertEquals(MutableVector(), eval(sexp))

        sexp = "(begin (define v (list 1 2 3))" +
                "       (vector-fill! v 3)" +
                "       v)"
        try {
            eval(sexp)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("vector-fill!: type mismatch; (expected: MutableVector, given: (1 2 3))", e.message)
        }
    }

    @Test
    fun testMutability() {
        assertEquals(true,  eval("(mutable?   [1 2 3])"))
        assertEquals(false, eval("(mutable?  #(1 2 3))"))
        assertEquals(true,  eval("(mutable?   (vector 1 2 3))"))
        assertEquals(false, eval("(immutable? (vector 1 2 3))"))
        assertEquals(true,  eval("(immutable? (vector->immutable-vector (vector 1 2 3)))"))
    }

    @Test
    fun testVectorsAsFunctionsOfIndex() {
        assertEquals(5L, eval("([0 5 10] 1)"))
        assertEquals(5L, eval("((vector 0 (+ 2 3) 10) 1)"))
        try {
            eval("([0 (+ 2 3) 10] 10)")
            fail()
        } catch (e: IndexOutOfBoundsException) {
            // expected
        }
    }

    @Test
    fun testVectorNext() {
        assertEquals(null, eval("(next [])"))
        assertEquals(null, eval("(next [1])"))
        assertEquals(listOf(2L, 3L), (eval("(next [1 2 3])") as Sequence<*>).toList())
    }

    @Test
    fun testVectorRest() {
        assertEquals(emptyList<Any?>(), (eval("(rest [])") as Sequence<*>).toList())
        assertEquals(emptyList<Any?>(), (eval("(rest [1])") as Sequence<*>).toList())
        assertEquals(listOf(2L, 3L),    (eval("(rest [1 2 3])") as Sequence<*>).toList())
    }
}
