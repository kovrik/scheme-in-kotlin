package unittests;

import core.scm.SCMNil;
import org.junit.Test;

import java.util.ArrayList;

import static core.procedures.cons.Car.car;
import static core.procedures.cons.Cdr.cdr;
import static core.procedures.cons.ConsProc.cons;
import static core.procedures.predicates.SCMPredicate.IS_LIST;
import static core.procedures.predicates.SCMPredicate.IS_NULL;
import static core.procedures.predicates.SCMPredicate.IS_PAIR;
import static core.scm.SCMCons.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static java.lang.Boolean.TRUE;
import static java.lang.Boolean.FALSE;

public class SCMConsTest {

  @Test
  public void testEquality() {
    assertTrue(EMPTY == cons(null, null));
    assertTrue(cons(null, null) == cons(null, null));
    assertEquals(EMPTY, cons(null, null));
    assertEquals(cons(null, null), cons(null, null));
    assertEquals(cons(1, 2), cons(1, 2));
    assertFalse(cons(1, 2) == cons(1, 2));
  }

  @Test
  public void testToString() {
    assertEquals("()", EMPTY.toString());
    assertEquals("()", cons(null, null).toString());
    assertEquals("(1 . 2)", cons(1, 2).toString());
    assertEquals("(1 2 . 3)", cons(1, cons(2, 3)).toString());
    assertEquals("(1 2 3 . 4)", cons(1, cons(2, (cons(3, 4)))).toString());
    assertEquals("(1)", cons(1, EMPTY).toString());
    assertEquals("(1 2)", cons(1, cons(2, EMPTY)).toString());
    assertEquals("(1 2 3)", cons(1, cons(2, cons(3, EMPTY))).toString());
    assertEquals("(1 2 3 4)", cons(1, cons(2, cons(3, cons(4, EMPTY)))).toString());
    assertEquals("(())", cons(EMPTY, EMPTY).toString());
    assertEquals("(() ())", cons(EMPTY, cons(EMPTY, EMPTY)).toString());
    assertEquals("(() . 1)", cons(EMPTY, 1).toString());
    assertEquals("(() (1 2 ()) ())", list(EMPTY, list(1, 2, EMPTY), EMPTY).toString());
    assertEquals("()", list().toString());
    assertEquals("(1)", list(1).toString());
    assertEquals("(1)", cons(1, new ArrayList()).toString());
    assertEquals("(2 1)", cons(2, cons(1, new ArrayList())).toString());
    assertEquals("(1 2)", list(1, 2).toString());
    assertEquals("(1 2 3 4)", list(1, 2, 3, 4).toString());
    assertEquals("(1 2 () 4)", list(1, 2, EMPTY, 4).toString());
  }

  @Test
  public void testList() {
    assertEquals(cons(1, cons(2, cons(3, EMPTY))), list(1, 2, 3));
    assertEquals(list(1, 2, 3) ,list(1, 2, 3));
    assertEquals(cons(1, EMPTY), list(1));
    assertEquals(EMPTY, list());
    assertEquals(cons(EMPTY, cons(EMPTY, cons(EMPTY, EMPTY))), list(EMPTY, EMPTY, EMPTY));
  }

  @Test
  public void testCar() {
    assertEquals(1, car(cons(1, 2)));
    assertEquals(1, car(cons(1, cons(2, 3))));
    assertEquals(EMPTY, car(cons(EMPTY, cons(2, 3))));

    assertEquals(1, car(list(1)));
    assertEquals(1, car(list(1, 2)));
    assertEquals(1, car(list(1, 2, 3)));

    assertEquals(2, car(cdr(list(1, 2, 3))));
    assertEquals(3, car(cdr(cdr(list(1, 2, 3)))));

    try {
      car(EMPTY);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("car: type mismatch; (expected: Pair, given: ())", e.getMessage());
    }
    try {
      car(list());
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("car: type mismatch; (expected: Pair, given: ())", e.getMessage());
    }
  }

  @Test
  public void testCdr() {
    assertEquals(2, car(cdr(cons(1, cons(2, 3)))));
    assertEquals(3, cdr(cdr(cons(1, cons(2, 3)))));
    assertEquals(3, cdr(cdr(cons(1, cons(2, 3)))));
    assertEquals(EMPTY, cdr(list(1)));
    assertEquals(EMPTY, cdr(list((Object)EMPTY)));
    assertEquals(list(2, 3), cdr(list(1, 2, 3)));
    assertEquals(list(3), cdr(cdr(list(1, 2, 3))));
    assertEquals(EMPTY, cdr(cdr(cdr(list(1, 2, 3)))));
  }

  @Test
  public void testLength() {
    assertEquals(0, list().size());
    assertEquals(1, list(1).size());
    assertEquals(2, list(EMPTY, cons(EMPTY, EMPTY)).size());
    assertEquals(3, list(1, EMPTY, 3).size());
    assertEquals(9, list(1, EMPTY, 3, 4, 5, 6, 777, 88, 99999).size());
  }

  @Test
  public void testIsNil() {
    assertEquals(TRUE,  IS_NULL.apply(SCMNil.NIL));
    assertEquals(FALSE, IS_NULL.apply(EMPTY));
    assertEquals(FALSE, IS_NULL.apply(cons(1, null)));
    assertEquals(FALSE, IS_NULL.apply(cons(null, 2)));
    assertEquals(FALSE, IS_NULL.apply(cons(EMPTY, 2)));
    assertEquals(FALSE, IS_NULL.apply(cons(EMPTY, EMPTY)));
    assertEquals(FALSE, IS_NULL.apply(cons(1, EMPTY)));
    assertEquals(FALSE, IS_NULL.apply(cons(1, cons(2, 3))));
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
    assertEquals(FALSE, IS_PAIR.apply(cons(null, null)));
    assertEquals(FALSE, IS_PAIR.apply(1));
    assertEquals(FALSE, IS_PAIR.apply("test"));
    assertEquals(TRUE, IS_PAIR.apply(cons(1, 2)));
    assertEquals(TRUE, IS_PAIR.apply(cons(1, EMPTY)));
    assertEquals(TRUE, IS_PAIR.apply(cons(EMPTY, EMPTY)));
    assertEquals(TRUE, IS_PAIR.apply(cons(1, cons(2, 3))));
    assertEquals(TRUE, IS_PAIR.apply(cons(1, cons(2, cons(3, 4)))));
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
    assertEquals(TRUE,  IS_LIST.apply(cons(null, null)));
    assertEquals(TRUE,  IS_LIST.apply(cons(1, null)));
    assertEquals(FALSE, IS_LIST.apply(cons(null, 1)));
    assertEquals(FALSE, IS_LIST.apply(cons(1, 2)));
    assertEquals(FALSE, IS_LIST.apply(cons(1, cons(2, cons(3, 4)))));
    assertEquals(TRUE,  IS_LIST.apply(cons(1, cons(2, cons(3, EMPTY)))));
    assertEquals(TRUE,  IS_LIST.apply(cons(1, cons(2, cons(3, list(1, 2, 3))))));
  }
}
