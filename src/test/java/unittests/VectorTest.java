package unittests;

import core.scm.SCMClass;
import core.scm.SCMMutableVector;
import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMCons.list;
import static core.scm.SCMUnspecified.UNSPECIFIED;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class VectorTest extends AbstractTest {

  @Test
  public void testEvalIsVector() {
    assertEquals(FALSE, eval("(vector? #\\A)", env));
    assertEquals(TRUE, eval("(vector? #(1 2 3 ))", env));
  }

  @Test
  public void testEvalVector() {
    assertEquals(new SCMMutableVector(), eval("#()", env));
    assertEquals(new SCMMutableVector(1L, 2L, 3L), eval("#(1 2 3 )", env));

    assertEquals(new SCMMutableVector(), eval("(vector)", env));
    assertEquals(new SCMMutableVector(1L, 2L, 3L), eval("(vector 1 2 3)", env));
    assertEquals(new SCMMutableVector(1L, 2L, 3L), eval("(vector 1 2 (+ 1 2))", env));
  }

  @Test
  public void testEvalMakeVector() {
    assertEquals(new SCMMutableVector(1L, 1L, 1L), eval("(make-vector 3 1)", env));
    assertEquals(new SCMMutableVector(), eval("(make-vector 0)", env));
    assertEquals(new SCMMutableVector(UNSPECIFIED, UNSPECIFIED, UNSPECIFIED), eval("(make-vector 3)", env));
    try {
      eval("(make-vector 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong number of arguments (3) passed to: make-vector", e.getMessage());
    }

    try {
      eval("(make-vector \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals(String.format("Wrong argument type. Expected: %s, actual: \"test\"",
                                 SCMClass.ExactNonNegativeInteger.class.getSimpleName()), e.getMessage());
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
      assertEquals("Wrong argument type. Expected: Vector, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalVectorRef() {
    assertEquals(1L, eval("(vector-ref (vector 1 2 3) 0)", env));
    assertEquals(2L, eval("(vector-ref (vector 1 2 3) 1)", env));
    assertEquals(3L, eval("(vector-ref (vector 1 2 3) 2)", env));
    assertEquals("test", eval("(vector-ref (vector \"test\" 2 3) 0)", env));

    try {
      eval("(vector-ref (vector 1 2 3) -1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals(String.format("Wrong argument type. Expected: %s, actual: -1",
                                 SCMClass.ExactNonNegativeInteger.class.getSimpleName()), e.getMessage());
    }
    try {
      eval("(vector-ref (vector 1 2 3) 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Value out of range: 3", e.getMessage());
    }
    try {
      eval("(vector-ref (vector) 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Value out of range: 0", e.getMessage());
    }
    try {
      eval("(vector-ref '(1 2 3) 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: MutableVector, actual: (1 2 3)", e.getMessage());
    }
    try {
      eval("(vector-ref (vector 1 2 3) 0.5)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals(String.format("Wrong argument type. Expected: %s, actual: 0.5",
                                 SCMClass.ExactNonNegativeInteger.class.getSimpleName()), e.getMessage());
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
    assertEquals("test", eval(sexp, env));

    sexp = "(begin (define v (vector 1 2 3)) (vector-set! v -1 \"test\"))";
    try {
      eval(sexp, env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals(String.format("Wrong argument type. Expected: %s, actual: -1",
                                 SCMClass.ExactNonNegativeInteger.class.getSimpleName()), e.getMessage());
    }

    sexp = "(begin (define v (vector 1 2 3)) (vector-set! v 3 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertEquals("Value out of range: 3", e.getMessage());
    }

    sexp = "(begin (define v (vector))" +
        "       (vector-set! v 0 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertEquals("Value out of range: 0", e.getMessage());
    }

    sexp = "(begin (define v '(1 2 3))" +
        "       (vector-set! v 0 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: MutableVector, actual: (1 2 3)", e.getMessage());
    }

    sexp = "(begin (define v (vector 1 2)) (vector-set! v 0.5 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertEquals(String.format("Wrong argument type. Expected: %s, actual: 0.5",
                                 SCMClass.ExactNonNegativeInteger.class.getSimpleName()), e.getMessage());
    }
  }

  @Test
  public void testEvalVectorToList() {

    assertEquals(list(1L, 2L, "test"), eval("(vector->list #(1 2 \"test\"))", env));
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
    assertEquals(new SCMMutableVector(3L, 3L, 3L), eval(sexp, env));

    sexp = "(begin (define v (vector))" +
        "       (vector-fill! v 3)" +
        "       v)";
    assertEquals(new SCMMutableVector(), eval(sexp, env));

    sexp = "(begin (define v (list 1 2 3))" +
        "       (vector-fill! v 3)" +
        "       v)";
    try {
      eval(sexp, env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: MutableVector, actual: (1 2 3)", e.getMessage());
    }
  }

  @Test
  public void testMutability() {
    assertEquals(TRUE,  eval("(mutable? (vector 1 2 3))", env));
    assertEquals(FALSE, eval("(immutable? (vector 1 2 3))", env));
    assertEquals(TRUE,  eval("(immutable? (vector->immutable-vector (vector 1 2 3)))", env));
  }
}
