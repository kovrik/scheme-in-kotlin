import core.scm.IList;
import core.scm.IPair;
import core.scm.SCMList;
import org.junit.Test;

import static core.procedures.cons.ConsProc.cons;
import static core.procedures.cons.IsNull.isNull;
import static core.procedures.cons.IsPair.isPair;
import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMList.NIL;
import static junit.framework.Assert.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static org.junit.Assert.*;

public class SCMConsTest {

//  @Test
//  public void tetToString() {
//
//    assertEquals("(1 . 2)", cons(1, 2).toString());
//    assertEquals("(1 2 . 3)", cons(1, cons(2, 3)).toString());
//    assertEquals("(1 2 3 . 4)", cons(1, cons(2, cons(3, 4))).toString());
//  }

  @Test
  public void testEquality() {
    assertTrue(NIL == cons(null, null));
    assertTrue(cons(null, null) == cons(null, null));
    assertEquals(NIL, cons(null, null));
    assertEquals(cons(null, null), cons(null, null));
    assertNotEquals(cons(1, 2), cons(1, 2));
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
    assertEquals("(() (1 2 ()) ())", new SCMList(NIL, new SCMList(1, 2, NIL), NIL).toString());
    assertEquals("()", new SCMList().toString());
    assertEquals("(1)", new SCMList(1).toString());
    assertEquals("(1 2)", new SCMList(1, 2).toString());
    assertEquals("(1 2 3 4)", new SCMList(1, 2, 3, 4).toString());
    assertEquals("(1 2 () 4)", new SCMList(1, 2, NIL, 4).toString());
  }

  @Test
  public void testList() {
    assertEquals(cons(1, cons(2, cons(3, NIL))), new SCMList(1, 2, 3));
    assertEquals(new SCMList(1, 2, 3) ,new SCMList(1, 2, 3));
    assertEquals(cons(1, NIL), new SCMList(1));
    assertEquals(NIL, new SCMList());
    assertEquals(cons(NIL, cons(NIL, cons(NIL, NIL))), new SCMList(NIL, NIL, NIL));
  }

  @Test
  public void testCar() {
    assertEquals(1, car(cons(1, 2)));
    assertEquals(1, car(cons(1, cons(2, 3))));
    assertEquals(NIL, car(cons(NIL, cons(2, 3))));

    assertEquals(1, car(new SCMList(1)));
    assertEquals(1, car(new SCMList(1, 2)));
    assertEquals(1, car(new SCMList(1, 2, 3)));

    assertEquals(2, car(cdr(new SCMList(1, 2, 3))));
    assertEquals(3, car(cdr(cdr(new SCMList(1, 2, 3)))));

    try {
      car(NIL);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: '()", e.getMessage());
    }
    try {
      car(new SCMList());
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: '()", e.getMessage());
    }
  }

  @Test
  public void testCdr() {
    assertEquals(2, car(cdr(cons(1, cons(2, 3)))));
    assertEquals(3, cdr(cdr(cons(1, cons(2, 3)))));
    assertEquals(3, cdr(cdr(cons(1, cons(2, 3)))));
    assertEquals(NIL, cdr(new SCMList(1)));
    assertEquals(NIL, cdr(new SCMList((Object)NIL)));
    assertEquals(new SCMList(2, 3), cdr(new SCMList(1, 2, 3)));
    assertEquals(new SCMList(3), cdr(cdr(new SCMList(1, 2, 3))));
    assertEquals(NIL, cdr(cdr(cdr(new SCMList(1, 2, 3)))));
  }

  @Test
  public void testLength() {
    assertEquals(0, length(new SCMList()));
    assertEquals(1, length(new SCMList(1)));
    assertEquals(2, length(new SCMList(NIL, cons(NIL, NIL))));
    assertEquals(3, length(new SCMList(1, NIL, 3)));
    assertEquals(9, length(new SCMList(1, NIL, 3, 4, 5, 6, 777, 88, 99999)));
  }

  @Test
  public void testIsNil() {
    assertEquals(TRUE,  isNull(NIL));
    assertEquals(TRUE,  isNull(cons(null, null)));
    assertEquals(TRUE,  isNull(new SCMList()));
    assertEquals(FALSE, isNull(cons(1, null)));
    assertEquals(FALSE, isNull(cons(null, 2)));
    assertEquals(FALSE, isNull(cons(NIL, 2)));
    assertEquals(FALSE, isNull(cons(NIL, NIL)));
    assertEquals(FALSE, isNull(cons(1, NIL)));
    assertEquals(FALSE, isNull(cons(1, cons(2, 3))));
    assertEquals(FALSE, isNull(new SCMList((Object) null)));
    assertEquals(FALSE, isNull(new SCMList((Object)NIL)));
    assertEquals(FALSE, isNull(new SCMList(1)));
    assertEquals(FALSE, isNull(new SCMList(1, 2)));
    assertEquals(FALSE, isNull(new SCMList(1, 2, 3)));
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
    assertEquals(FALSE, isPair(new SCMList()));
    assertEquals(TRUE, isPair(new SCMList(1)));
    assertEquals(TRUE, isPair(new SCMList(1, 2)));
    assertEquals(TRUE, isPair(new SCMList(1, 2, 3)));
  }

  // TODO
//  @Test
//  public void testIsList() {
//    assertTrue(isList(NIL));
//    assertTrue(isList(new SCMList()));
//    assertTrue(isList(new SCMList(1)));
//    assertTrue(isList(new SCMList(1, 2)));
//    assertTrue(isList(new SCMList(1, 2, 3)));
//    assertTrue(isList(new SCMList(1, 2, 3, 4)));
//    assertTrue(isList(new SCMList(1, 2, NIL)));
//    assertTrue(isList(cons(null, null)));
//    assertTrue(isList(cons(1, null)));
//    assertFalse(isList(cons(null, 1)));
//    assertFalse(isList(cons(1, 2)));
//    assertFalse(isList(cons(1, cons(2, cons(3, 4)))));
//    assertTrue(isList(cons(1, cons(2, cons(3, NIL)))));
//    assertTrue(isList(cons(1, cons(2, cons(3, new SCMList(1, 2, 3))))));
//  }

  public static Object car(Object pair) {
    return ((IPair)pair).car();
  }

  public static Object cdr(Object pair) {
    return ((IPair)pair).cdr();
  }

  public static long length(Object list) {
    return ((IList)list).length();
  }
}
