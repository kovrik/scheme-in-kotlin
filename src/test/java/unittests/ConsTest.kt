package unittests

import core.procedures.cons.Car
import core.procedures.cons.Cdr
import core.procedures.cons.ConsProc
import core.procedures.predicates.Predicate
import core.writer.Writer
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
        assertEquals("(1 . 2)", Writer.write(cons(1, 2)))
        assertEquals("(1 2 . 3)", Writer.write(cons(1, cons(2, 3))))
        assertEquals("(1 2 3 . 4)", Writer.write(cons(1, cons(2, cons(3, 4)))))
        assertEquals("(1)",       Writer.write(cons(1, emptyList<Nothing>())))
        assertEquals("(1 2)",     Writer.write(cons(1, cons(2, emptyList<Nothing>()))))
        assertEquals("(1 2 3)",   Writer.write(cons(1, cons(2, cons(3, emptyList<Nothing>())))))
        assertEquals("(1 2 3 4)", Writer.write(cons(1, cons(2, cons(3, cons(4, emptyList<Nothing>()))))))
        assertEquals("(())",      Writer.write(cons(emptyList<Nothing>(), emptyList<Nothing>())))
        assertEquals("(() ())",   Writer.write(cons(emptyList<Nothing>(), cons(emptyList<Nothing>(), emptyList<Nothing>()))))
        assertEquals("(() . 1)",  Writer.write(cons(emptyList<Nothing>(), 1)))
        assertEquals("(() (1 2 ()) ())", Writer.write(listOf(emptyList<Nothing>(), listOf(1, 2, emptyList<Nothing>()), emptyList<Nothing>())))
        assertEquals("()",    Writer.write(listOf<Any>()))
        assertEquals("(1)",   Writer.write(listOf(1)))
        assertEquals("(1)",   Writer.write(cons(1, emptyList<Any?>())))
        assertEquals("(2 1)", Writer.write(cons(2, cons(1, emptyList<Any?>()))))
        assertEquals("(1 2)", Writer.write(listOf(1, 2)))
        assertEquals("(1 2 3 4)",  Writer.write(listOf(1, 2, 3, 4)))
        assertEquals("(1 2 () 4)", Writer.write(listOf(1, 2, emptyList<Nothing>(), 4)))
    }

    @Test
    fun testList() {
        assertEquals(cons(1, cons(2, cons(3, emptyList<Nothing>()))), listOf(1, 2, 3))
        assertEquals(listOf(1, 2, 3), listOf(1, 2, 3))
        assertEquals(cons(1, emptyList<Nothing>()), listOf(1))
        assertEquals(emptyList<Nothing>(), listOf<Any>())
        assertEquals(cons(emptyList<Nothing>(), cons(emptyList<Nothing>(), cons(emptyList<Nothing>(), emptyList<Nothing>()))), listOf(emptyList<Nothing>(), emptyList<Nothing>(), emptyList<Nothing>()))
    }

    @Test
    fun testCar() {
        assertEquals(1, car(cons(1, 2)))
        assertEquals(1, car(cons(1, cons(2, 3))))
        assertEquals(emptyList<Nothing>(), car(cons(emptyList<Nothing>(), cons(2, 3))))

        assertEquals(1, car(listOf(1)))
        assertEquals(1, car(listOf(1, 2)))
        assertEquals(1, car(listOf(1, 2, 3)))

        assertEquals(2, car(cdr(listOf(1, 2, 3))))
        assertEquals(3, car(cdr(cdr(listOf(1, 2, 3)))))
    }

    @Test
    fun testCdr() {
        assertEquals(2, car(cdr(cons(1, cons(2, 3)))))
        assertEquals(3, cdr(cdr(cons(1, cons(2, 3)))))
        assertEquals(3, cdr(cdr(cons(1, cons(2, 3)))))
        assertEquals(emptyList<Nothing>(), cdr(listOf(1)))
        assertEquals(emptyList<Nothing>(), cdr(listOf(emptyList<Nothing>() as Any)))
        assertEquals(listOf(2, 3), cdr(listOf(1, 2, 3)))
        assertEquals(listOf(3), cdr(cdr(listOf(1, 2, 3))))
        assertEquals(emptyList<Nothing>(), cdr(cdr(cdr(listOf(1, 2, 3)))))
    }

    @Test
    fun testLength() {
        assertEquals(0, listOf<Any>().size.toLong())
        assertEquals(1, listOf(1).size.toLong())
        assertEquals(2, listOf<List<*>>(emptyList<Nothing>(), cons(emptyList<Nothing>(), emptyList<Nothing>())).size.toLong())
        assertEquals(3, listOf(1, emptyList<Nothing>(), 3).size.toLong())
    }

    @Test
    fun testIsNil() {
        assertEquals(true,  Predicate.IS_NULL(null))
        assertEquals(false, Predicate.IS_NULL(emptyList<Nothing>()))
        assertEquals(false, Predicate.IS_NULL(cons(1, null)))
        assertEquals(false, Predicate.IS_NULL(cons(emptyList<Nothing>(), 2)))
        assertEquals(false, Predicate.IS_NULL(cons(emptyList<Nothing>(), emptyList<Nothing>())))
        assertEquals(false, Predicate.IS_NULL(cons(1, emptyList<Nothing>())))
        assertEquals(false, Predicate.IS_NULL(cons(1, cons(2, 3))))
        assertEquals(false, Predicate.IS_NULL(listOf<Any?>(null)))
        assertEquals(false, Predicate.IS_NULL(listOf(emptyList<Nothing>())))
        assertEquals(false, Predicate.IS_NULL(listOf(1)))
        assertEquals(false, Predicate.IS_NULL(listOf(1, 2)))
        assertEquals(false, Predicate.IS_NULL(listOf(1, 2, 3)))
        assertEquals(false, Predicate.IS_NULL(1))
        assertEquals(false, Predicate.IS_NULL("test"))
    }

    @Test
    fun testIsPair() {
        assertEquals(false, Predicate.IS_PAIR(emptyList<Nothing>()))
        assertEquals(false, Predicate.IS_PAIR(1))
        assertEquals(false, Predicate.IS_PAIR("test"))
        assertEquals(false, Predicate.IS_PAIR(listOf<Any>()))
        assertEquals(true,  Predicate.IS_PAIR(cons(null, null)))
        assertEquals(true,  Predicate.IS_PAIR(cons(1, 2)))
        assertEquals(true,  Predicate.IS_PAIR(cons(1, emptyList<Nothing>())))
        assertEquals(true,  Predicate.IS_PAIR(cons(emptyList<Nothing>(), emptyList<Nothing>())))
        assertEquals(true,  Predicate.IS_PAIR(cons(1, cons(2, 3))))
        assertEquals(true,  Predicate.IS_PAIR(cons(1, cons(2, cons(3, 4)))))
        assertEquals(true,  Predicate.IS_PAIR(listOf(1)))
        assertEquals(true,  Predicate.IS_PAIR(listOf(1, 2)))
        assertEquals(true,  Predicate.IS_PAIR(listOf(1, 2, 3)))
    }

    @Test
    fun testIsList() {
        assertEquals(true,  Predicate.IS_LIST(emptyList<Nothing>()))
        assertEquals(true,  Predicate.IS_LIST(listOf<Any>()))
        assertEquals(true,  Predicate.IS_LIST(listOf(1)))
        assertEquals(true,  Predicate.IS_LIST(listOf(1, 2)))
        assertEquals(true,  Predicate.IS_LIST(listOf(1, 2, 3)))
        assertEquals(true,  Predicate.IS_LIST(listOf(1, 2, 3, 4)))
        assertEquals(true,  Predicate.IS_LIST(listOf(1, 2, emptyList<Nothing>())))
        assertEquals(true,  Predicate.IS_LIST(cons(null, null)))
        assertEquals(true,  Predicate.IS_LIST(cons(1, null)))
        assertEquals(false, Predicate.IS_LIST(cons(1, 2)))
        assertEquals(false, Predicate.IS_LIST(cons(1, cons(2, cons(3, 4)))))
        assertEquals(true,  Predicate.IS_LIST(cons(1, cons(2, cons(3, emptyList<Nothing>())))))
        assertEquals(true,  Predicate.IS_LIST(cons(1, cons(2, cons(3, listOf(1, 2, 3))))))
    }
}
