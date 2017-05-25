package unittests

import core.procedures.cons.Car
import core.procedures.cons.Cdr
import core.procedures.cons.ConsProc
import core.procedures.predicates.Predicate.Companion
import core.scm.Cons
import core.scm.Cons.Companion.EMPTY
import core.scm.Cons.Companion.list
import org.junit.Assert.*
import org.junit.Test
import java.lang.Boolean.FALSE
import java.lang.Boolean.TRUE
import java.util.*

class ConsTest {

    @Test
    fun testEquality() {
        assertEquals(ConsProc.cons(null, null), ConsProc.cons(null, null))
        assertEquals(ConsProc.cons(1, 2), ConsProc.cons(1, 2))
        assertFalse(ConsProc.cons(1, 2) === ConsProc.cons(1, 2))
    }

    @Test
    fun testToString() {
        assertEquals("()", EMPTY.toString())
        assertEquals("(1 . 2)", ConsProc.cons(1, 2).toString())
        assertEquals("(1 2 . 3)", ConsProc.cons(1, ConsProc.cons(2, 3)).toString())
        assertEquals("(1 2 3 . 4)", ConsProc.cons(1, ConsProc.cons(2, ConsProc.cons(3, 4))).toString())
        assertEquals("(1)", ConsProc.cons(1, EMPTY).toString())
        assertEquals("(1 2)", ConsProc.cons(1, ConsProc.cons(2, EMPTY)).toString())
        assertEquals("(1 2 3)", ConsProc.cons(1, ConsProc.cons(2, ConsProc.cons(3, EMPTY))).toString())
        assertEquals("(1 2 3 4)", ConsProc.cons(1, ConsProc.cons(2, ConsProc.cons(3, ConsProc.cons(4, EMPTY)))).toString())
        assertEquals("(())", ConsProc.cons(EMPTY, EMPTY).toString())
        assertEquals("(() ())", ConsProc.cons(EMPTY, ConsProc.cons(EMPTY, EMPTY)).toString())
        assertEquals("(() . 1)", ConsProc.cons(EMPTY, 1).toString())
        assertEquals("(() (1 2 ()) ())", list<Cons<Any?>>(EMPTY, list(1, 2, EMPTY), EMPTY).toString())
        assertEquals("()", list<Any>().toString())
        assertEquals("(1)", list(1).toString())
        assertEquals("(1)", ConsProc.cons(1, Collections.EMPTY_LIST).toString())
        assertEquals("(2 1)", ConsProc.cons(2, ConsProc.cons(1, Collections.EMPTY_LIST)).toString())
        assertEquals("(1 2)", list(1, 2).toString())
        assertEquals("(1 2 3 4)", list(1, 2, 3, 4).toString())
        assertEquals("(1 2 () 4)", list(1, 2, EMPTY, 4).toString())
    }

    @Test
    fun testList() {
        assertEquals(ConsProc.cons(1, ConsProc.cons(2, ConsProc.cons(3, EMPTY))), list(1, 2, 3))
        assertEquals(list(1, 2, 3), list(1, 2, 3))
        assertEquals(ConsProc.cons(1, EMPTY), list(1))
        assertEquals(EMPTY, list<Any>())
        assertEquals(ConsProc.cons(EMPTY, ConsProc.cons(EMPTY, ConsProc.cons(EMPTY, EMPTY))), list(EMPTY, EMPTY, EMPTY))
    }

    @Test
    fun testCar() {
        assertEquals(1, Car.car(ConsProc.cons(1, 2)))
        assertEquals(1, Car.car(ConsProc.cons(1, ConsProc.cons(2, 3))))
        assertEquals(EMPTY, Car.car(ConsProc.cons(EMPTY, ConsProc.cons(2, 3))))

        assertEquals(1, Car.car(list(1)))
        assertEquals(1, Car.car(list(1, 2)))
        assertEquals(1, Car.car(list(1, 2, 3)))

        assertEquals(2, Car.car(Cdr.cdr(list(1, 2, 3))))
        assertEquals(3, Car.car(Cdr.cdr(Cdr.cdr(list(1, 2, 3)))))

        try {
            Car.car(EMPTY)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("car: type mismatch; (expected: Pair, given: ())", e.message)
        }

        try {
            Car.car(list<Any>())
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("car: type mismatch; (expected: Pair, given: ())", e.message)
        }
    }

    @Test
    fun testCdr() {
        assertEquals(2, Car.car(Cdr.cdr(ConsProc.cons(1, ConsProc.cons(2, 3)))))
        assertEquals(3, Cdr.cdr(Cdr.cdr(ConsProc.cons(1, ConsProc.cons(2, 3)))))
        assertEquals(3, Cdr.cdr(Cdr.cdr(ConsProc.cons(1, ConsProc.cons(2, 3)))))
        assertEquals(EMPTY, Cdr.cdr(list(1)))
        assertEquals(EMPTY, Cdr.cdr(list(EMPTY as Any)))
        assertEquals(list(2, 3), Cdr.cdr(list(1, 2, 3)))
        assertEquals(list(3), Cdr.cdr(Cdr.cdr(list(1, 2, 3))))
        assertEquals(EMPTY, Cdr.cdr(Cdr.cdr(Cdr.cdr(list(1, 2, 3)))))
    }

    @Test
    fun testLength() {
        assertEquals(0, list<Any>().size.toLong())
        assertEquals(1, list(1).size.toLong())
        assertEquals(2, list<Cons<*>>(EMPTY, ConsProc.cons(EMPTY, EMPTY)).size.toLong())
        assertEquals(3, list(1, EMPTY, 3).size.toLong())
        assertEquals(9, list(1, EMPTY, 3, 4, 5, 6, 777, 88, 99999).size.toLong())
    }

    @Test
    fun testIsNil() {
        assertEquals(TRUE, Companion.IS_NULL.invoke(null))
        assertEquals(FALSE, Companion.IS_NULL.invoke(EMPTY))
        assertEquals(FALSE, Companion.IS_NULL.invoke(ConsProc.cons(1, null)))
        assertEquals(FALSE, Companion.IS_NULL.invoke(ConsProc.cons(EMPTY, 2)))
        assertEquals(FALSE, Companion.IS_NULL.invoke(ConsProc.cons(EMPTY, EMPTY)))
        assertEquals(FALSE, Companion.IS_NULL.invoke(ConsProc.cons(1, EMPTY)))
        assertEquals(FALSE, Companion.IS_NULL.invoke(ConsProc.cons(1, ConsProc.cons(2, 3))))
        assertEquals(FALSE, Companion.IS_NULL.invoke(list<Any?>(null)))
        assertEquals(FALSE, Companion.IS_NULL.invoke(list(EMPTY as Any)))
        assertEquals(FALSE, Companion.IS_NULL.invoke(list(1)))
        assertEquals(FALSE, Companion.IS_NULL.invoke(list(1, 2)))
        assertEquals(FALSE, Companion.IS_NULL.invoke(list(1, 2, 3)))
        assertEquals(FALSE, Companion.IS_NULL.invoke(1))
        assertEquals(FALSE, Companion.IS_NULL.invoke("test"))
    }

    @Test
    fun testIsPair() {
        assertEquals(FALSE, Companion.IS_PAIR.invoke(EMPTY))
        assertEquals(FALSE, Companion.IS_PAIR.invoke(1))
        assertEquals(FALSE, Companion.IS_PAIR.invoke("test"))
        assertEquals(FALSE, Companion.IS_PAIR.invoke(list<Any>()))
        assertEquals(TRUE,  Companion.IS_PAIR.invoke(ConsProc.cons(null, null)))
        assertEquals(TRUE,  Companion.IS_PAIR.invoke(ConsProc.cons(1, 2)))
        assertEquals(TRUE,  Companion.IS_PAIR.invoke(ConsProc.cons(1, EMPTY)))
        assertEquals(TRUE,  Companion.IS_PAIR.invoke(ConsProc.cons(EMPTY, EMPTY)))
        assertEquals(TRUE,  Companion.IS_PAIR.invoke(ConsProc.cons(1, ConsProc.cons(2, 3))))
        assertEquals(TRUE,  Companion.IS_PAIR.invoke(ConsProc.cons(1, ConsProc.cons(2, ConsProc.cons(3, 4)))))
        assertEquals(TRUE,  Companion.IS_PAIR.invoke(list(1)))
        assertEquals(TRUE,  Companion.IS_PAIR.invoke(list(1, 2)))
        assertEquals(TRUE,  Companion.IS_PAIR.invoke(list(1, 2, 3)))
    }

    @Test
    fun testIsList() {
        assertEquals(TRUE, Companion.IS_LIST.invoke(EMPTY))
        assertEquals(TRUE, Companion.IS_LIST.invoke(list<Any>()))
        assertEquals(TRUE, Companion.IS_LIST.invoke(list(1)))
        assertEquals(TRUE, Companion.IS_LIST.invoke(list(1, 2)))
        assertEquals(TRUE, Companion.IS_LIST.invoke(list(1, 2, 3)))
        assertEquals(TRUE, Companion.IS_LIST.invoke(list(1, 2, 3, 4)))
        assertEquals(TRUE, Companion.IS_LIST.invoke(list(1, 2, EMPTY)))
        assertEquals(TRUE, Companion.IS_LIST.invoke(ConsProc.cons(null, null)))
        assertEquals(TRUE, Companion.IS_LIST.invoke(ConsProc.cons(1, null)))
        assertEquals(FALSE, Companion.IS_LIST.invoke(ConsProc.cons(1, 2)))
        assertEquals(FALSE, Companion.IS_LIST.invoke(ConsProc.cons(1, ConsProc.cons(2, ConsProc.cons(3, 4)))))
        assertEquals(TRUE, Companion.IS_LIST.invoke(ConsProc.cons(1, ConsProc.cons(2, ConsProc.cons(3, EMPTY)))))
        assertEquals(TRUE, Companion.IS_LIST.invoke(ConsProc.cons(1, ConsProc.cons(2, ConsProc.cons(3, list(1, 2, 3))))))
    }
}
