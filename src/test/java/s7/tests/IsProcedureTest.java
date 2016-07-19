package s7.tests;

import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
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
    } catch (IllegalSyntaxException e) {
      assertEquals("Bad syntax in form: begin", e.getMessage());
    }
    try {
      eval("(procedure? lambda)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Bad syntax in form: lambda", e.getMessage());
    }
    assertEquals(FALSE, eval("(procedure? 'and)", env));
    assertEquals(FALSE, eval("(procedure? 'let)", env));
    assertEquals(FALSE, eval("(procedure? 'quasiquote)", env));
    assertEquals(FALSE, eval("(procedure? 'cond)", env));
    assertEquals(FALSE, eval("(procedure? 'do)", env));
    assertEquals(FALSE, eval("(procedure? 'set!)", env));
    assertEquals(FALSE, eval("(procedure? \"hi\")", env));
    assertEquals(FALSE, eval("(procedure? '(1 2))", env));
    assertEquals(FALSE, eval("(procedure? #(1 2))", env));
    try {
      eval("(procedure? and)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Bad syntax in form: and", e.getMessage());
    }
    try {
      eval("(procedure? let)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Bad syntax in form: let", e.getMessage());
    }
    try {
      eval("(procedure? quasiquote)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Bad syntax in form: quasiquote", e.getMessage());
    }
    try {
      eval("(procedure? cond)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Bad syntax in form: cond", e.getMessage());
    }
    try {
      eval("(procedure? do)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Bad syntax in form: do", e.getMessage());
    }
    try {
      eval("(procedure? set!)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Bad syntax in form: set!", e.getMessage());
    }
    try {
      eval("(procedure?)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 0, expected: 1) passed to: procedure?", e.getMessage());
    }
    try {
      eval("(procedure? abs car)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 2, expected: 1) passed to: procedure?", e.getMessage());
    }
  }
}
