package unittests;

import core.procedures.cons.Car;
import core.procedures.cons.Cdr;
import core.procedures.cons.ConsProc;
import org.junit.Test;

import java.util.Collections;

import static core.procedures.predicates.Predicate.Companion;
import static core.scm.Cons.EMPTY;
import static core.scm.Cons.list;
import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class ConsTest {

  @Test
  public void testEquality() {
    assertTrue(EMPTY == ConsProc.Companion.cons(null, null));
    assertTrue(ConsProc.Companion.cons(null, null) == ConsProc.Companion.cons(null, null));
    assertEquals(EMPTY, ConsProc.Companion.cons(null, null));
    assertEquals(ConsProc.Companion.cons(null, null), ConsProc.Companion.cons(null, null));
    assertEquals(ConsProc.Companion.cons(1, 2), ConsProc.Companion.cons(1, 2));
    assertFalse(ConsProc.Companion.cons(1, 2) == ConsProc.Companion.cons(1, 2));
  }

  @Test
  public void testToString() {
    assertEquals("()", EMPTY.toString());
    assertEquals("()", ConsProc.Companion.cons(null, null).toString());
    assertEquals("(1 . 2)", ConsProc.Companion.cons(1, 2).toString());
    assertEquals("(1 2 . 3)", ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, 3)).toString());
    assertEquals("(1 2 3 . 4)", ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, (ConsProc.Companion.cons(3, 4)))).toString());
    assertEquals("(1)", ConsProc.Companion.cons(1, EMPTY).toString());
    assertEquals("(1 2)", ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, EMPTY)).toString());
    assertEquals("(1 2 3)", ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, ConsProc.Companion.cons(3, EMPTY))).toString());
    assertEquals("(1 2 3 4)", ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, ConsProc.Companion.cons(3, ConsProc.Companion.cons(4, EMPTY)))).toString());
    assertEquals("(())", ConsProc.Companion.cons(EMPTY, EMPTY).toString());
    assertEquals("(() ())", ConsProc.Companion.cons(EMPTY, ConsProc.Companion.cons(EMPTY, EMPTY)).toString());
    assertEquals("(() . 1)", ConsProc.Companion.cons(EMPTY, 1).toString());
    assertEquals("(() (1 2 ()) ())", list(EMPTY, list(1, 2, EMPTY), EMPTY).toString());
    assertEquals("()", list().toString());
    assertEquals("(1)", list(1).toString());
    assertEquals("(1)", ConsProc.Companion.cons(1, Collections.EMPTY_LIST).toString());
    assertEquals("(2 1)", ConsProc.Companion.cons(2, ConsProc.Companion.cons(1, Collections.EMPTY_LIST)).toString());
    assertEquals("(1 2)", list(1, 2).toString());
    assertEquals("(1 2 3 4)", list(1, 2, 3, 4).toString());
    assertEquals("(1 2 () 4)", list(1, 2, EMPTY, 4).toString());
  }

  @Test
  public void testList() {
    assertEquals(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, ConsProc.Companion.cons(3, EMPTY))), list(1, 2, 3));
    assertEquals(list(1, 2, 3) ,list(1, 2, 3));
    assertEquals(ConsProc.Companion.cons(1, EMPTY), list(1));
    assertEquals(EMPTY, list());
    assertEquals(ConsProc.Companion.cons(EMPTY, ConsProc.Companion.cons(EMPTY, ConsProc.Companion.cons(EMPTY, EMPTY))), list(EMPTY, EMPTY, EMPTY));
  }

  @Test
  public void testCar() {
    assertEquals(1, Car.Companion.car(ConsProc.Companion.cons(1, 2)));
    assertEquals(1, Car.Companion.car(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, 3))));
    assertEquals(EMPTY, Car.Companion.car(ConsProc.Companion.cons(EMPTY, ConsProc.Companion.cons(2, 3))));

    assertEquals(1, Car.Companion.car(list(1)));
    assertEquals(1, Car.Companion.car(list(1, 2)));
    assertEquals(1, Car.Companion.car(list(1, 2, 3)));

    assertEquals(2, Car.Companion.car(Cdr.Companion.cdr(list(1, 2, 3))));
    assertEquals(3, Car.Companion.car(Cdr.Companion.cdr(Cdr.Companion.cdr(list(1, 2, 3)))));

    try {
      Car.Companion.car(EMPTY);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("car: type mismatch; (expected: Pair, given: ())", e.getMessage());
    }
    try {
      Car.Companion.car(list());
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("car: type mismatch; (expected: Pair, given: ())", e.getMessage());
    }
  }

  @Test
  public void testCdr() {
    assertEquals(2, Car.Companion.car(Cdr.Companion.cdr(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, 3)))));
    assertEquals(3, Cdr.Companion.cdr(Cdr.Companion.cdr(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, 3)))));
    assertEquals(3, Cdr.Companion.cdr(Cdr.Companion.cdr(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, 3)))));
    assertEquals(EMPTY, Cdr.Companion.cdr(list(1)));
    assertEquals(EMPTY, Cdr.Companion.cdr(list((Object)EMPTY)));
    assertEquals(list(2, 3), Cdr.Companion.cdr(list(1, 2, 3)));
    assertEquals(list(3), Cdr.Companion.cdr(Cdr.Companion.cdr(list(1, 2, 3))));
    assertEquals(EMPTY, Cdr.Companion.cdr(Cdr.Companion.cdr(Cdr.Companion.cdr(list(1, 2, 3)))));
  }

  @Test
  public void testLength() {
    assertEquals(0, list().size());
    assertEquals(1, list(1).size());
    assertEquals(2, list(EMPTY, ConsProc.Companion.cons(EMPTY, EMPTY)).size());
    assertEquals(3, list(1, EMPTY, 3).size());
    assertEquals(9, list(1, EMPTY, 3, 4, 5, 6, 777, 88, 99999).size());
  }

  @Test
  public void testIsNil() {
    assertEquals(TRUE,  Companion.getIS_NULL().apply1(null));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(EMPTY));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(ConsProc.Companion.cons(1, null)));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(ConsProc.Companion.cons(null, 2)));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(ConsProc.Companion.cons(EMPTY, 2)));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(ConsProc.Companion.cons(EMPTY, EMPTY)));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(ConsProc.Companion.cons(1, EMPTY)));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, 3))));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(list((Object) null)));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(list((Object)EMPTY)));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(list(1)));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(list(1, 2)));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(list(1, 2, 3)));
    assertEquals(FALSE, Companion.getIS_NULL().apply1(1));
    assertEquals(FALSE, Companion.getIS_NULL().apply1("test"));
  }

  @Test
  public void testIsPair() {
    assertEquals(FALSE, Companion.getIS_PAIR().apply1(EMPTY));
    assertEquals(FALSE, Companion.getIS_PAIR().apply1(ConsProc.Companion.cons(null, null)));
    assertEquals(FALSE, Companion.getIS_PAIR().apply1(1));
    assertEquals(FALSE, Companion.getIS_PAIR().apply1("test"));
    assertEquals(TRUE,  Companion.getIS_PAIR().apply1(ConsProc.Companion.cons(1, 2)));
    assertEquals(TRUE,  Companion.getIS_PAIR().apply1(ConsProc.Companion.cons(1, EMPTY)));
    assertEquals(TRUE,  Companion.getIS_PAIR().apply1(ConsProc.Companion.cons(EMPTY, EMPTY)));
    assertEquals(TRUE,  Companion.getIS_PAIR().apply1(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, 3))));
    assertEquals(TRUE,  Companion.getIS_PAIR().apply1(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, ConsProc.Companion.cons(3, 4)))));
    assertEquals(FALSE, Companion.getIS_PAIR().apply1(list()));
    assertEquals(TRUE,  Companion.getIS_PAIR().apply1(list(1)));
    assertEquals(TRUE,  Companion.getIS_PAIR().apply1(list(1, 2)));
    assertEquals(TRUE,  Companion.getIS_PAIR().apply1(list(1, 2, 3)));
  }

  @Test
  public void testIsList() {
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(EMPTY));
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(list()));
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(list(1)));
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(list(1, 2)));
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(list(1, 2, 3)));
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(list(1, 2, 3, 4)));
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(list(1, 2, EMPTY)));
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(ConsProc.Companion.cons(null, null)));
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(ConsProc.Companion.cons(1, null)));
    assertEquals(FALSE, Companion.getIS_LIST().apply1(ConsProc.Companion.cons(null, 1)));
    assertEquals(FALSE, Companion.getIS_LIST().apply1(ConsProc.Companion.cons(1, 2)));
    assertEquals(FALSE, Companion.getIS_LIST().apply1(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, ConsProc.Companion.cons(3, 4)))));
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, ConsProc.Companion.cons(3, EMPTY)))));
    assertEquals(TRUE,  Companion.getIS_LIST().apply1(ConsProc.Companion.cons(1, ConsProc.Companion.cons(2, ConsProc.Companion.cons(3, list(1, 2, 3))))));
  }
}
