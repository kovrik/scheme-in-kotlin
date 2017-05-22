package unittests;

import core.procedures.cons.Car;
import core.procedures.cons.Cdr;
import org.junit.Test;

import java.util.Collections;

import static core.procedures.cons.ConsProc.Companion;
import static core.procedures.predicates.Predicate.*;
import static core.scm.Cons.EMPTY;
import static core.scm.Cons.list;
import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.*;

public class ConsTest {

  @Test
  public void testEquality() {
    assertTrue(EMPTY == Companion.cons(null, null));
    assertTrue(Companion.cons(null, null) == Companion.cons(null, null));
    assertEquals(EMPTY, Companion.cons(null, null));
    assertEquals(Companion.cons(null, null), Companion.cons(null, null));
    assertEquals(Companion.cons(1, 2), Companion.cons(1, 2));
    assertFalse(Companion.cons(1, 2) == Companion.cons(1, 2));
  }

  @Test
  public void testToString() {
    assertEquals("()", EMPTY.toString());
    assertEquals("()", Companion.cons(null, null).toString());
    assertEquals("(1 . 2)", Companion.cons(1, 2).toString());
    assertEquals("(1 2 . 3)", Companion.cons(1, Companion.cons(2, 3)).toString());
    assertEquals("(1 2 3 . 4)", Companion.cons(1, Companion.cons(2, (Companion.cons(3, 4)))).toString());
    assertEquals("(1)", Companion.cons(1, EMPTY).toString());
    assertEquals("(1 2)", Companion.cons(1, Companion.cons(2, EMPTY)).toString());
    assertEquals("(1 2 3)", Companion.cons(1, Companion.cons(2, Companion.cons(3, EMPTY))).toString());
    assertEquals("(1 2 3 4)", Companion.cons(1, Companion.cons(2, Companion.cons(3, Companion.cons(4, EMPTY)))).toString());
    assertEquals("(())", Companion.cons(EMPTY, EMPTY).toString());
    assertEquals("(() ())", Companion.cons(EMPTY, Companion.cons(EMPTY, EMPTY)).toString());
    assertEquals("(() . 1)", Companion.cons(EMPTY, 1).toString());
    assertEquals("(() (1 2 ()) ())", list(EMPTY, list(1, 2, EMPTY), EMPTY).toString());
    assertEquals("()", list().toString());
    assertEquals("(1)", list(1).toString());
    assertEquals("(1)", Companion.cons(1, Collections.EMPTY_LIST).toString());
    assertEquals("(2 1)", Companion.cons(2, Companion.cons(1, Collections.EMPTY_LIST)).toString());
    assertEquals("(1 2)", list(1, 2).toString());
    assertEquals("(1 2 3 4)", list(1, 2, 3, 4).toString());
    assertEquals("(1 2 () 4)", list(1, 2, EMPTY, 4).toString());
  }

  @Test
  public void testList() {
    assertEquals(Companion.cons(1, Companion.cons(2, Companion.cons(3, EMPTY))), list(1, 2, 3));
    assertEquals(list(1, 2, 3) ,list(1, 2, 3));
    assertEquals(Companion.cons(1, EMPTY), list(1));
    assertEquals(EMPTY, list());
    assertEquals(Companion.cons(EMPTY, Companion.cons(EMPTY, Companion.cons(EMPTY, EMPTY))), list(EMPTY, EMPTY, EMPTY));
  }

  @Test
  public void testCar() {
    assertEquals(1, Car.Companion.car(Companion.cons(1, 2)));
    assertEquals(1, Car.Companion.car(Companion.cons(1, Companion.cons(2, 3))));
    assertEquals(EMPTY, Car.Companion.car(Companion.cons(EMPTY, Companion.cons(2, 3))));

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
    assertEquals(2, Car.Companion.car(Cdr.Companion.cdr(Companion.cons(1, Companion.cons(2, 3)))));
    assertEquals(3, Cdr.Companion.cdr(Cdr.Companion.cdr(Companion.cons(1, Companion.cons(2, 3)))));
    assertEquals(3, Cdr.Companion.cdr(Cdr.Companion.cdr(Companion.cons(1, Companion.cons(2, 3)))));
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
    assertEquals(2, list(EMPTY, Companion.cons(EMPTY, EMPTY)).size());
    assertEquals(3, list(1, EMPTY, 3).size());
    assertEquals(9, list(1, EMPTY, 3, 4, 5, 6, 777, 88, 99999).size());
  }

  @Test
  public void testIsNil() {
    assertEquals(TRUE,  IS_NULL.apply((Object)null));
    assertEquals(FALSE, IS_NULL.apply(EMPTY));
    assertEquals(FALSE, IS_NULL.apply(Companion.cons(1, null)));
    assertEquals(FALSE, IS_NULL.apply(Companion.cons(null, 2)));
    assertEquals(FALSE, IS_NULL.apply(Companion.cons(EMPTY, 2)));
    assertEquals(FALSE, IS_NULL.apply(Companion.cons(EMPTY, EMPTY)));
    assertEquals(FALSE, IS_NULL.apply(Companion.cons(1, EMPTY)));
    assertEquals(FALSE, IS_NULL.apply(Companion.cons(1, Companion.cons(2, 3))));
    assertEquals(FALSE, IS_NULL.apply(list((Object) null)));
    assertEquals(FALSE, IS_NULL.apply(list((Object)EMPTY)));
    assertEquals(FALSE, IS_NULL.apply(list(1)));
    assertEquals(FALSE, IS_NULL.apply(list(1, 2)));
    assertEquals(FALSE, IS_NULL.apply(list(1, 2, 3)));
    assertEquals(FALSE, IS_NULL.apply(1));
    assertEquals(FALSE, IS_NULL.apply("test"));
  }

  @Test
  public void testIsPair() {
    assertEquals(FALSE, IS_PAIR.apply(EMPTY));
    assertEquals(FALSE, IS_PAIR.apply(Companion.cons(null, null)));
    assertEquals(FALSE, IS_PAIR.apply(1));
    assertEquals(FALSE, IS_PAIR.apply("test"));
    assertEquals(TRUE, IS_PAIR.apply(Companion.cons(1, 2)));
    assertEquals(TRUE, IS_PAIR.apply(Companion.cons(1, EMPTY)));
    assertEquals(TRUE, IS_PAIR.apply(Companion.cons(EMPTY, EMPTY)));
    assertEquals(TRUE, IS_PAIR.apply(Companion.cons(1, Companion.cons(2, 3))));
    assertEquals(TRUE, IS_PAIR.apply(Companion.cons(1, Companion.cons(2, Companion.cons(3, 4)))));
    assertEquals(FALSE, IS_PAIR.apply(list()));
    assertEquals(TRUE, IS_PAIR.apply(list(1)));
    assertEquals(TRUE, IS_PAIR.apply(list(1, 2)));
    assertEquals(TRUE, IS_PAIR.apply(list(1, 2, 3)));
  }

  @Test
  public void testIsList() {
    assertEquals(TRUE,  IS_LIST.apply(EMPTY));
    assertEquals(TRUE,  IS_LIST.apply(list()));
    assertEquals(TRUE,  IS_LIST.apply(list(1)));
    assertEquals(TRUE,  IS_LIST.apply(list(1, 2)));
    assertEquals(TRUE,  IS_LIST.apply(list(1, 2, 3)));
    assertEquals(TRUE,  IS_LIST.apply(list(1, 2, 3, 4)));
    assertEquals(TRUE,  IS_LIST.apply(list(1, 2, EMPTY)));
    assertEquals(TRUE,  IS_LIST.apply(Companion.cons(null, null)));
    assertEquals(TRUE,  IS_LIST.apply(Companion.cons(1, null)));
    assertEquals(FALSE, IS_LIST.apply(Companion.cons(null, 1)));
    assertEquals(FALSE, IS_LIST.apply(Companion.cons(1, 2)));
    assertEquals(FALSE, IS_LIST.apply(Companion.cons(1, Companion.cons(2, Companion.cons(3, 4)))));
    assertEquals(TRUE,  IS_LIST.apply(Companion.cons(1, Companion.cons(2, Companion.cons(3, EMPTY)))));
    assertEquals(TRUE,  IS_LIST.apply(Companion.cons(1, Companion.cons(2, Companion.cons(3, list(1, 2, 3))))));
  }
}
