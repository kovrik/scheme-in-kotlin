package unittests;

import core.scm.SCMMutableString;
import core.scm.SCMVector;
import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMCons.list;
import static core.scm.SCMUnspecified.UNSPECIFIED;
import static org.junit.Assert.*;

public class VectorTest extends AbstractTest {

  @Test
  public void testEvalIsVector() {
    assertEquals(FALSE, eval("(vector? #\\A)", env));
    assertEquals(TRUE, eval("(vector? #(1 2 3 ))", env));
  }

  @Test
  public void testEvalVector() {
    assertEquals(new SCMVector(), eval("#()", env));
    assertEquals(new SCMVector(1L, 2L, 3L), eval("#(1 2 3 )", env));

    assertEquals(new SCMVector(), eval("(vector)", env));
    assertEquals(new SCMVector(1L, 2L, 3L), eval("(vector 1 2 3)", env));
    assertEquals(new SCMVector(1L, 2L, 3L), eval("(vector 1 2 (+ 1 2))", env));
  }

  @Test
  public void testEvalMakeVector() {
    assertEquals(new SCMVector(1L, 1L, 1L), eval("(make-vector 3 1)", env));
    assertEquals(new SCMVector(), eval("(make-vector 0)", env));
    assertEquals(new SCMVector(UNSPECIFIED, UNSPECIFIED, UNSPECIFIED), eval("(make-vector 3)", env));
    try {
      eval("(make-vector 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong number of arguments (3) passed to: make-vector"));
    }

    try {
      eval("(make-vector \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: \"test\""));
    }
  }

  @Test
  public void testEvalVectorLength() {
    assertEquals(0L, eval("(vector-length #())", env));
    assertEquals(0L, eval("(vector-length (vector))", env));
    assertEquals(3L, eval("(vector-length (vector 1 2 3))", env));

    try {
      eval("(vector-length 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Vector, actual: 1"));
    }
  }

  @Test
  public void testEvalVectorRef() {
    assertEquals(1L, eval("(vector-ref (vector 1 2 3) 0)", env));
    assertEquals(2L, eval("(vector-ref (vector 1 2 3) 1)", env));
    assertEquals(3L, eval("(vector-ref (vector 1 2 3) 2)", env));
    assertEquals(new SCMMutableString("test"), eval("(vector-ref (vector \"test\" 2 3) 0)", env));

    try {
      eval("(vector-ref (vector 1 2 3) -1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: -1"));
    }
    try {
      eval("(vector-ref (vector 1 2 3) 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 3"));
    }
    try {
      eval("(vector-ref (vector) 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 0"));
    }
    try {
      eval("(vector-ref '(1 2 3) 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Vector, actual: (1 2 3)"));
    }
    try {
      eval("(vector-ref (vector 1 2 3) 0.5)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: 0.5"));
    }
  }

  @Test
  public void testEvalVectorSet() {

    String sexp = "(begin (define v (vector 1 2 3))" +
        "       (vector-set! v 0 99)" +
        "       (vector-ref  v 0))";
    assertEquals(99L, eval(sexp, env));

    sexp = "(begin (define v (vector 1 2 3))" +
        "       (vector-set! v 2 \"test\")" +
        "       (vector-ref  v 2))";
    assertEquals(new SCMMutableString("test"), eval(sexp, env));

    sexp = "(begin (define v (vector 1 2 3))" +
        "       (vector-set! v -1 \"test\"))";
    try {
      eval(sexp, env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: -1"));
    }

    sexp = "(begin (define v (vector 1 2 3))" +
        "       (vector-set! v 3 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 3"));
    }

    sexp = "(begin (define v (vector))" +
        "       (vector-set! v 0 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 0"));
    }

    sexp = "(begin (define v '(1 2 3))" +
        "       (vector-set! v 0 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Vector, actual: (1 2 3)"));
    }

    sexp = "(begin (define v (vector 1 2))" +
        "       (vector-set! v 0.5 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: 0.5"));
    }
  }

  @Test
  public void testEvalVectorToList() {

    assertEquals(list(1L, 2L, new SCMMutableString("test")), eval("(vector->list #(1 2 \"test\"))", env));
    assertEquals(list(), eval("(vector->list #())", env));

    try {
      eval("(vector->list '(1 2 3))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Vector, actual: (1 2 3)", e.getMessage());
    }
  }

  @Test
  public void testEvalVectorFill() {

    String sexp = "(begin (define v (vector 1 2 3))" +
        "       (vector-fill! v 3)" +
        "       v)";
    assertEquals(new SCMVector(3L, 3L, 3L), eval(sexp, env));

    sexp = "(begin (define v (vector))" +
        "       (vector-fill! v 3)" +
        "       v)";
    assertEquals(new SCMVector(), eval(sexp, env));

    sexp = "(begin (define v (list 1 2 3))" +
        "       (vector-fill! v 3)" +
        "       v)";
    try {
      eval(sexp, env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Vector, actual: (1 2 3)", e.getMessage());
    }
  }

}
