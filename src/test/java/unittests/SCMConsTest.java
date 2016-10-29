package unittests;

import org.junit.Test;

import java.util.ArrayList;

import static core.procedures.cons.Car.car;
import static core.procedures.cons.Cdr.cdr;
import static core.procedures.cons.ConsProc.cons;
import static core.procedures.predicates.IsList.isList;
import static core.procedures.predicates.IsNull.isNull;
import static core.procedures.predicates.IsPair.isPair;
import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMCons.NIL;
import static core.scm.SCMCons.list;
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
    assertEquals(TRUE,  isNull(NIL));
    assertEquals(TRUE,  isNull(cons(null, null)));
    assertEquals(TRUE,  isNull(list()));
    assertEquals(FALSE, isNull(cons(1, null)));
    assertEquals(FALSE, isNull(cons(null, 2)));
    assertEquals(FALSE, isNull(cons(NIL, 2)));
    assertEquals(FALSE, isNull(cons(NIL, NIL)));
    assertEquals(FALSE, isNull(cons(1, NIL)));
    assertEquals(FALSE, isNull(cons(1, cons(2, 3))));
    assertEquals(FALSE, isNull(list((Object) null)));
    assertEquals(FALSE, isNull(list((Object)NIL)));
    assertEquals(FALSE, isNull(list(1)));
    assertEquals(FALSE, isNull(list(1, 2)));
    assertEquals(FALSE, isNull(list(1, 2, 3)));
    assertEquals(FALSE, isNull(1));
    assertEquals(FALSE, isNull("test"));
  }

  @Test
  public void testIsPair() {
    assertEquals(FALSE, isPair(NIL));
    assertEquals(FALSE, isPair(cons(null, null)));
    assertEquals(FALSE, isPair(1));
    assertEquals(FALSE, isPair("test"));
    assertEquals(TRUE, isPair(cons(1, 2)));
    assertEquals(TRUE, isPair(cons(1, NIL)));
    assertEquals(TRUE, isPair(cons(NIL, NIL)));
    assertEquals(TRUE, isPair(cons(1, cons(2, 3))));
    assertEquals(TRUE, isPair(cons(1, cons(2, cons(3, 4)))));
    assertEquals(FALSE, isPair(list()));
    assertEquals(TRUE, isPair(list(1)));
    assertEquals(TRUE, isPair(list(1, 2)));
    assertEquals(TRUE, isPair(list(1, 2, 3)));
  }

  @Test
  public void testIsList() {
    assertEquals(TRUE,  isList(NIL));
    assertEquals(TRUE,  isList(list()));
    assertEquals(TRUE,  isList(list(1)));
    assertEquals(TRUE,  isList(list(1, 2)));
    assertEquals(TRUE,  isList(list(1, 2, 3)));
    assertEquals(TRUE,  isList(list(1, 2, 3, 4)));
    assertEquals(TRUE,  isList(list(1, 2, NIL)));
    assertEquals(TRUE,  isList(cons(null, null)));
    assertEquals(TRUE,  isList(cons(1, null)));
    assertEquals(FALSE, isList(cons(null, 1)));
    assertEquals(FALSE, isList(cons(1, 2)));
    assertEquals(FALSE, isList(cons(1, cons(2, cons(3, 4)))));
    assertEquals(TRUE,  isList(cons(1, cons(2, cons(3, NIL)))));
    assertEquals(TRUE,  isList(cons(1, cons(2, cons(3, list(1, 2, 3))))));
  }
}
