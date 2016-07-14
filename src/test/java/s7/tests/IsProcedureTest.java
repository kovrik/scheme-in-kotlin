package s7.tests;

import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.fail;

public class IsProcedureTest extends AbstractS7Test {

  @Test
  public void testIsProcedure() {
    assertEquals(TRUE, eval("(procedure? car)", env));
    assertEquals(TRUE, eval("(procedure? procedure?)", env));
    assertEquals(FALSE, eval("(procedure? 'car)", env));
    assertEquals(TRUE, eval("(procedure? (lambda (x) x))", env));
    assertEquals(FALSE, eval("(procedure? '(lambda (x) x))", env));
    assertEquals(TRUE, eval("(let ((a (lambda (x) x)))	(procedure? a))", env));
    assertEquals(TRUE, eval("(letrec ((a (lambda () (procedure? a)))) (a))", env));
    assertEquals(FALSE, eval("(let ((a 1)) (let ((a (lambda () (procedure? a)))) (a)))", env));
    assertEquals(TRUE, eval("(let () (define (hi) 1) (procedure? hi))", env));
    try {
      eval("(procedure? begin)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Bad syntax in form: begin", e.getMessage());
    }
    try {
      eval("(procedure? lambda)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Bad syntax in form: lambda", e.getMessage());
    }
    assertEquals(FALSE, eval("(procedure? 'and)", env));
    assertEquals(FALSE, eval("(procedure? 'let)", env));
    assertEquals(FALSE, eval("(procedure? 'quasiquote)", env));
    assertEquals(FALSE, eval("(procedure? 'cond)", env));
    assertEquals(FALSE, eval("(procedure? 'do)", env));
    assertEquals(FALSE, eval("(procedure? 'set!)", env));

    try {
      eval("(procedure? and)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Bad syntax in form: and", e.getMessage());
    }

    try {
      eval("(procedure? let)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Bad syntax in form: let", e.getMessage());
    }

    try {
      eval("(procedure? quasiquote)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Bad syntax in form: quasiquote", e.getMessage());
    }

    try {
      eval("(procedure? cond)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Bad syntax in form: cond", e.getMessage());
    }

    try {
      eval("(procedure? do)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Bad syntax in form: do", e.getMessage());
    }

    try {
      eval("(procedure? set!)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Bad syntax in form: set!", e.getMessage());
    }

//    (procedure?) 'error)
//    (procedure? abs car) 'error)
//    (procedure abs) 'error)

//    ;; these are questionable -- an applicable object is a procedure
//    (test (procedure? "hi") #f)
//    (test (procedure? '(1 2)) #f)
//    (test (procedure? #(1 2)) #f)

  }
}
