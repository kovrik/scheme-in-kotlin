package unittests;

import org.junit.Test;

import java.util.ArrayList;

import static core.procedures.cons.Car.car;
import static core.procedures.cons.Cdr.cdr;
import static core.procedures.cons.ConsProc.cons;
import static core.procedures.predicates.SCMPredicate.IS_LIST;
import static core.procedures.predicates.SCMPredicate.IS_NULL;
import static core.procedures.predicates.SCMPredicate.IS_PAIR;
import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMCons.*;
import static junit.framework.Assert.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class SCMConsTest {

  @Test
  public void testEquality() {
    assertTrue(NIL == cons(null, null));
    assertTrue(cons(null, null) == cons(null, null));
    assertEquals(NIL, cons(null, null));
    assertEquals(cons(null, null), cons(null, null));
    assertEquals(cons(1, 2), cons(1, 2));
    assertFalse(cons(1, 2) == cons(1, 2));
  }

  @Test
  public void testToString() {
    assertEquals("()", NIL.toString());
    assertEquals("()", cons(null, null).toString());
    assertEquals("(1 . 2)", cons(1, 2).toString());
    assertEquals("(1 2 . 3)", cons(1, cons(2, 3)).toString());
    assertEquals("(1 2 3 . 4)", cons(1, cons(2, (cons(3, 4)))).toString());
    assertEquals("(1)", cons(1, NIL).toString());
    assertEquals("(1 2)", cons(1, cons(2, NIL)).toString());
    assertEquals("(1 2 3)", cons(1, cons(2, cons(3, NIL))).toString());
    assertEquals("(1 2 3 4)", cons(1, cons(2, cons(3, cons(4, NIL)))).toString());
    assertEquals("(())", cons(NIL, NIL).toString());
    assertEquals("(() ())", cons(NIL, cons(NIL, NIL)).toString());
    assertEquals("(() . 1)", cons(NIL, 1).toString());
    assertEquals("(() (1 2 ()) ())", list(NIL, list(1, 2, NIL), NIL).toString());
    assertEquals("()", list().toString());
    assertEquals("(1)", list(1).toString());
    assertEquals("(1)", cons(1, new ArrayList()).toString());
    assertEquals("(2 1)", cons(2, cons(1, new ArrayList())).toString());
    assertEquals("(1 2)", list(1, 2).toString());
    assertEquals("(1 2 3 4)", list(1, 2, 3, 4).toString());
    assertEquals("(1 2 () 4)", list(1, 2, NIL, 4).toString());
  }

  @Test
  public void testList() {
    assertEquals(cons(1, cons(2, cons(3, NIL))), list(1, 2, 3));
    assertEquals(list(1, 2, 3) ,list(1, 2, 3));
    assertEquals(cons(1, NIL), list(1));
    assertEquals(NIL, list());
    assertEquals(cons(NIL, cons(NIL, cons(NIL, NIL))), list(NIL, NIL, NIL));
  }

  @Test
  public void testCar() {
    assertEquals(1, car(cons(1, 2)));
    assertEquals(1, car(cons(1, cons(2, 3))));
    assertEquals(NIL, car(cons(NIL, cons(2, 3))));

    assertEquals(1, car(list(1)));
    assertEquals(1, car(list(1, 2)));
    assertEquals(1, car(list(1, 2, 3)));

    assertEquals(2, car(cdr(list(1, 2, 3))));
    assertEquals(3, car(cdr(cdr(list(1, 2, 3)))));

    try {
      car(NIL);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: ()", e.getMessage());
    }
    try {
      car(list());
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: ()", e.getMessage());
    }
  }

  @Test
  public void testCdr() {
    assertEquals(2, car(cdr(cons(1, cons(2, 3)))));
    assertEquals(3, cdr(cdr(cons(1, cons(2, 3)))));
    assertEquals(3, cdr(cdr(cons(1, cons(2, 3)))));
    assertEquals(NIL, cdr(list(1)));
    assertEquals(NIL, cdr(list((Object)NIL)));
    assertEquals(list(2, 3), cdr(list(1, 2, 3)));
    assertEquals(list(3), cdr(cdr(list(1, 2, 3))));
    assertEquals(NIL, cdr(cdr(cdr(list(1, 2, 3)))));
  }

  @Test
  public void testLength() {
    assertEquals(0, list().size());
    assertEquals(1, list(1).size());
    assertEquals(2, list(NIL, cons(NIL, NIL)).size());
    assertEquals(3, list(1, NIL, 3).size());
    assertEquals(9, list(1, NIL, 3, 4, 5, 6, 777, 88, 99999).size());
  }

  @Test
  public void testIsNil() {
    assertEquals(TRUE,  IS_NULL.invoke(NIL));
    assertEquals(TRUE,  IS_NULL.invoke(cons(null, null)));
    assertEquals(TRUE,  IS_NULL.invoke(list()));
    assertEquals(FALSE, IS_NULL.invoke(cons(1, null)));
    assertEquals(FALSE, IS_NULL.invoke(cons(null, 2)));
    assertEquals(FALSE, IS_NULL.invoke(cons(NIL, 2)));
    assertEquals(FALSE, IS_NULL.invoke(cons(NIL, NIL)));
    assertEquals(FALSE, IS_NULL.invoke(cons(1, NIL)));
    assertEquals(FALSE, IS_NULL.invoke(cons(1, cons(2, 3))));
    assertEquals(FALSE, IS_NULL.invoke(list((Object) null)));
    assertEquals(FALSE, IS_NULL.invoke(list((Object)NIL)));
    assertEquals(FALSE, IS_NULL.invoke(list(1)));
    assertEquals(FALSE, IS_NULL.invoke(list(1, 2)));
    assertEquals(FALSE, IS_NULL.invoke(list(1, 2, 3)));
    assertEquals(FALSE, IS_NULL.invoke(1));
    assertEquals(FALSE, IS_NULL.invoke("test"));
  }

  @Test
  public void testIsPair() {
    assertEquals(FALSE, IS_PAIR.invoke(NIL));
    assertEquals(FALSE, IS_PAIR.invoke(cons(null, null)));
    assertEquals(FALSE, IS_PAIR.invoke(1));
    assertEquals(FALSE, IS_PAIR.invoke("test"));
    assertEquals(TRUE, IS_PAIR.invoke(cons(1, 2)));
    assertEquals(TRUE, IS_PAIR.invoke(cons(1, NIL)));
    assertEquals(TRUE, IS_PAIR.invoke(cons(NIL, NIL)));
    assertEquals(TRUE, IS_PAIR.invoke(cons(1, cons(2, 3))));
    assertEquals(TRUE, IS_PAIR.invoke(cons(1, cons(2, cons(3, 4)))));
    assertEquals(FALSE, IS_PAIR.invoke(list()));
    assertEquals(TRUE, IS_PAIR.invoke(list(1)));
    assertEquals(TRUE, IS_PAIR.invoke(list(1, 2)));
    assertEquals(TRUE, IS_PAIR.invoke(list(1, 2, 3)));
  }

  @Test
  public void testIsList() {
    assertEquals(TRUE,  IS_LIST.invoke(NIL));
    assertEquals(TRUE,  IS_LIST.invoke(list()));
    assertEquals(TRUE,  IS_LIST.invoke(list(1)));
    assertEquals(TRUE,  IS_LIST.invoke(list(1, 2)));
    assertEquals(TRUE,  IS_LIST.invoke(list(1, 2, 3)));
    assertEquals(TRUE,  IS_LIST.invoke(list(1, 2, 3, 4)));
    assertEquals(TRUE,  IS_LIST.invoke(list(1, 2, NIL)));
    assertEquals(TRUE,  IS_LIST.invoke(cons(null, null)));
    assertEquals(TRUE,  IS_LIST.invoke(cons(1, null)));
    assertEquals(FALSE, IS_LIST.invoke(cons(null, 1)));
    assertEquals(FALSE, IS_LIST.invoke(cons(1, 2)));
    assertEquals(FALSE, IS_LIST.invoke(cons(1, cons(2, cons(3, 4)))));
    assertEquals(TRUE,  IS_LIST.invoke(cons(1, cons(2, cons(3, NIL)))));
    assertEquals(TRUE,  IS_LIST.invoke(cons(1, cons(2, cons(3, list(1, 2, 3))))));
  }
}
