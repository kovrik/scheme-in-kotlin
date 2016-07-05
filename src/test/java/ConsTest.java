import core.scm.Cons;
import org.junit.Test;

import static core.scm.Cons.cons;
import static core.scm.Cons.NIL;
import static core.scm.Cons.list;
import static core.scm.Cons.car;
import static core.scm.Cons.cdr;
import static core.scm.Cons.isList;
import static core.scm.Cons.isNil;
import static core.scm.Cons.isPair;
import static core.scm.Cons.length;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static org.junit.Assert.assertTrue;

@Deprecated
public class ConsTest {

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
    assertEquals("(1 2)", list(1, 2).toString());
    assertEquals("(1 2 3 4)", list(1, 2, 3, 4).toString());
    assertEquals("(1 2 () 4)", list(1, 2, NIL, 4).toString());
  }

  @Test
  public void testList() {
    assertEquals(cons(1, cons(2, cons(3, NIL))), list(1, 2, 3));
    assertEquals(cons(1, NIL), list(1));
    assertEquals(NIL, list());
    assertEquals(cons(NIL, cons(NIL, cons(NIL, NIL))), list(NIL, NIL, NIL));
  }

  @Test
  public void testCar() {
    assertEquals(1, car(cons(1, 2)));
    assertEquals(1, car(cons(1, cons(2, 3))));
    assertEquals(null, car(NIL));
    assertEquals(NIL, car(cons(NIL, cons(2, 3))));

    assertEquals(null, car(list()));
    assertEquals(1,    car(list(1)));
    assertEquals(1,    car(list(1, 2)));
    assertEquals(1,    car(list(1, 2, 3)));
    assertEquals(2,    car((Cons)cdr(list(1, 2, 3))));
    assertEquals(3,    car((Cons)cdr((Cons)cdr(list(1, 2, 3)))));
  }

  @Test
  public void testCdr() {
    assertEquals(cons(2, 3), cdr(cons(1, cons(2, 3))));
    assertEquals(3, cdr((Cons)cdr(cons(1, cons(2, 3)))));
    assertEquals(NIL, cdr(list(1)));
    assertEquals(NIL, cdr(list(NIL)));
    assertEquals(list(2, 3), cdr(list(1, 2, 3)));
    assertEquals(list(3), cdr((Cons)cdr(list(1, 2, 3))));
    assertEquals(NIL, cdr((Cons)cdr((Cons)cdr(list(1, 2, 3)))));
  }

  @Test
  public void testLength() {
    assertEquals(0, length(list()));
    assertEquals(1, length(list(1)));
    assertEquals(2, length(list(NIL, cons(NIL, NIL))));
    assertEquals(3, length(list(1, NIL, 3)));
    assertEquals(9, length(list(1, NIL, 3, 4, 5, 6, 777, 88, 99999)));
  }

  @Test
  public void testIsNil() {
    assertTrue(isNil(NIL));
    assertTrue(isNil(cons(null, null)));
    assertTrue(isNil(list()));
    assertFalse(isNil(cons(1, null)));
    assertFalse(isNil(cons(null, 2)));
    assertFalse(isNil(cons(NIL, 2)));
    assertFalse(isNil(cons(NIL, NIL)));
    assertFalse(isNil(cons(1, NIL)));
    assertFalse(isNil(cons(1, cons(2, 3))));
    assertFalse(isNil(list((Object) null)));
    assertFalse(isNil(list(NIL)));
    assertFalse(isNil(list(1)));
    assertFalse(isNil(list(1, 2)));
    assertFalse(isNil(list(1, 2, 3)));
    assertFalse(isNil(1));
    assertFalse(isNil("test"));
  }

  @Test
  public void testIsPair() {
    assertFalse(isPair(NIL));
    assertFalse(isPair(cons(null, null)));
    assertFalse(isPair(1));
    assertFalse(isPair("test"));
    assertTrue(isPair(cons(1, 2)));
    assertTrue(isPair(cons(1, NIL)));
    assertTrue(isPair(cons(NIL, NIL)));
    assertTrue(isPair(cons(1, cons(2, 3))));
    assertTrue(isPair(cons(1, cons(2, cons(3, 4)))));
    assertFalse(isPair(list()));
    assertTrue(isPair(list(1)));
    assertTrue(isPair(list(1, 2)));
    assertTrue(isPair(list(1, 2, 3)));
  }

  @Test
  public void testIsList() {
    assertTrue(isList(NIL));
    assertTrue(isList(list()));
    assertTrue(isList(list(1)));
    assertTrue(isList(list(1, 2)));
    assertTrue(isList(list(1, 2, 3)));
    assertTrue(isList(list(1, 2, 3, 4)));
    assertTrue(isList(list(1, 2, NIL)));
    assertTrue(isList(cons(null, null)));
    assertTrue(isList(cons(1, null)));
    assertFalse(isList(cons(null, 1)));
    assertFalse(isList(cons(1, 2)));
    assertFalse(isList(cons(1, cons(2, cons(3, 4)))));
    assertTrue(isList(cons(1, cons(2, cons(3, NIL)))));
    assertTrue(isList(cons(1, cons(2, cons(3, list(1, 2, 3))))));
  }
}
