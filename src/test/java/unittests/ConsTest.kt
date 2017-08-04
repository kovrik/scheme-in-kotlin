package unittests

import core.procedures.cons.Car
import core.procedures.cons.Cdr
import core.procedures.cons.ConsProc
import core.procedures.predicates.Predicate
import core.scm.Cons
import core.scm.Cons.Companion.EMPTY
import core.scm.Cons.Companion.list
import org.junit.Assert.*
import org.junit.Test

class ConsTest {

    private val car  = Car()
    private val cdr  = Cdr()
    private val cons = ConsProc()

    @Test
    fun testEquality() {
        assertEquals(cons(null, null), cons(null, null))
        assertEquals(cons(1, 2), cons(1, 2))
        assertFalse(cons(1, 2) === cons(1, 2))
    }

    @Test
    fun testToString() {
        assertEquals("()", EMPTY.toString())
        assertEquals("(1 . 2)", cons(1, 2).toString())
        assertEquals("(1 2 . 3)", cons(1, cons(2, 3)).toString())
        assertEquals("(1 2 3 . 4)", cons(1, cons(2, cons(3, 4))).toString())
        assertEquals("(1)", cons(1, EMPTY).toString())
        assertEquals("(1 2)", cons(1, cons(2, EMPTY)).toString())
        assertEquals("(1 2 3)", cons(1, cons(2, cons(3, EMPTY))).toString())
        assertEquals("(1 2 3 4)", cons(1, cons(2, cons(3, cons(4, EMPTY)))).toString())
        assertEquals("(())", cons(EMPTY, EMPTY).toString())
        assertEquals("(() ())", cons(EMPTY, cons(EMPTY, EMPTY)).toString())
        assertEquals("(() . 1)", cons(EMPTY, 1).toString())
        assertEquals("(() (1 2 ()) ())", list(EMPTY, list(1, 2, EMPTY), EMPTY).toString())
        assertEquals("()", list<Any>().toString())
        assertEquals("(1)", list(1).toString())
        assertEquals("(1)", cons(1, emptyList<Any?>()).toString())
        assertEquals("(2 1)", cons(2, cons(1, emptyList<Any?>())).toString())
        assertEquals("(1 2)", list(1, 2).toString())
        assertEquals("(1 2 3 4)", list(1, 2, 3, 4).toString())
        assertEquals("(1 2 () 4)", list(1, 2, EMPTY, 4).toString())
    }

    @Test
    fun testList() {
        assertEquals(cons(1, cons(2, cons(3, EMPTY))), list(1, 2, 3))
        assertEquals(list(1, 2, 3), list(1, 2, 3))
        assertEquals(cons(1, EMPTY), list(1))
        assertEquals(EMPTY, list<Any>())
        assertEquals(cons(EMPTY, cons(EMPTY, cons(EMPTY, EMPTY))), list(EMPTY, EMPTY, EMPTY))
    }

    @Test
    fun testCar() {
        assertEquals(1, car(cons(1, 2)))
        assertEquals(1, car(cons(1, cons(2, 3))))
        assertEquals(EMPTY, car(cons(EMPTY, cons(2, 3))))

        assertEquals(1, car(list(1)))
        assertEquals(1, car(list(1, 2)))
        assertEquals(1, car(list(1, 2, 3)))

        assertEquals(2, car(cdr(list(1, 2, 3))))
        assertEquals(3, car(cdr(cdr(list(1, 2, 3)))))

        try {
            car(EMPTY)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("car: type mismatch; (expected: Pair, given: ())", e.message)
        }
        try {
            car(list<Any>())
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("car: type mismatch; (expected: Pair, given: ())", e.message)
        }
    }

    @Test
    fun testCdr() {
        assertEquals(2, car(cdr(cons(1, cons(2, 3)))))
        assertEquals(3, cdr(cdr(cons(1, cons(2, 3)))))
        assertEquals(3, cdr(cdr(cons(1, cons(2, 3)))))
        assertEquals(EMPTY, cdr(list(1)))
        assertEquals(EMPTY, cdr(list(EMPTY as Any)))
        assertEquals(list(2, 3), cdr(list(1, 2, 3)))
        assertEquals(list(3), cdr(cdr(list(1, 2, 3))))
        assertEquals(EMPTY, cdr(cdr(cdr(list(1, 2, 3)))))
    }

    @Test
    fun testLength() {
        assertEquals(0, list<Any>().size.toLong())
        assertEquals(1, list(1).size.toLong())
        assertEquals(2, list<Cons<*>>(EMPTY, cons(EMPTY, EMPTY)).size.toLong())
        assertEquals(3, list(1, EMPTY, 3).size.toLong())
        assertEquals(9, list(arrayOf<Any?>(1, EMPTY, 3, 4, 5, 6, 777, 88, 99999)).size.toLong())
    }

    @Test
    fun testIsNil() {
        assertEquals(true,  Predicate.IS_NULL(null))
        assertEquals(false, Predicate.IS_NULL(EMPTY))
        assertEquals(false, Predicate.IS_NULL(cons(1, null)))
        assertEquals(false, Predicate.IS_NULL(cons(EMPTY, 2)))
        assertEquals(false, Predicate.IS_NULL(cons(EMPTY, EMPTY)))
        assertEquals(false, Predicate.IS_NULL(cons(1, EMPTY)))
        assertEquals(false, Predicate.IS_NULL(cons(1, cons(2, 3))))
        assertEquals(false, Predicate.IS_NULL(list<Any?>(null)))
        assertEquals(false, Predicate.IS_NULL(list(EMPTY)))
        assertEquals(false, Predicate.IS_NULL(list(1)))
        assertEquals(false, Predicate.IS_NULL(list(1, 2)))
        assertEquals(false, Predicate.IS_NULL(list(1, 2, 3)))
        assertEquals(false, Predicate.IS_NULL(1))
        assertEquals(false, Predicate.IS_NULL("test"))
    }

    @Test
    fun testIsPair() {
        assertEquals(false, Predicate.IS_PAIR(EMPTY))
        assertEquals(false, Predicate.IS_PAIR(1))
        assertEquals(false, Predicate.IS_PAIR("test"))
        assertEquals(false, Predicate.IS_PAIR(list<Any>()))
        assertEquals(true,  Predicate.IS_PAIR(cons(null, null)))
        assertEquals(true,  Predicate.IS_PAIR(cons(1, 2)))
        assertEquals(true,  Predicate.IS_PAIR(cons(1, EMPTY)))
        assertEquals(true,  Predicate.IS_PAIR(cons(EMPTY, EMPTY)))
        assertEquals(true,  Predicate.IS_PAIR(cons(1, cons(2, 3))))
        assertEquals(true,  Predicate.IS_PAIR(cons(1, cons(2, cons(3, 4)))))
        assertEquals(true,  Predicate.IS_PAIR(list(1)))
        assertEquals(true,  Predicate.IS_PAIR(list(1, 2)))
        assertEquals(true,  Predicate.IS_PAIR(list(1, 2, 3)))
    }

    @Test
    fun testIsList() {
        assertEquals(true,  Predicate.IS_LIST(EMPTY))
        assertEquals(true,  Predicate.IS_LIST(list<Any>()))
        assertEquals(true,  Predicate.IS_LIST(list(1)))
        assertEquals(true,  Predicate.IS_LIST(list(1, 2)))
        assertEquals(true,  Predicate.IS_LIST(list(1, 2, 3)))
        assertEquals(true,  Predicate.IS_LIST(list(1, 2, 3, 4)))
        assertEquals(true,  Predicate.IS_LIST(list(1, 2, EMPTY)))
        assertEquals(true,  Predicate.IS_LIST(cons(null, null)))
        assertEquals(true,  Predicate.IS_LIST(cons(1, null)))
        assertEquals(false, Predicate.IS_LIST(cons(1, 2)))
        assertEquals(false, Predicate.IS_LIST(cons(1, cons(2, cons(3, 4)))))
        assertEquals(true,  Predicate.IS_LIST(cons(1, cons(2, cons(3, EMPTY)))))
        assertEquals(true,  Predicate.IS_LIST(cons(1, cons(2, cons(3, list(1, 2, 3))))))
    }
}
