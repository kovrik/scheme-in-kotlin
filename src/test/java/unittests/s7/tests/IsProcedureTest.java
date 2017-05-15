package unittests.s7.tests;

import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import org.junit.Test;
import unittests.AbstractTest;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class IsProcedureTest extends AbstractTest {

  @Test
  public void testIsProcedure() {
    assertEquals(TRUE, eval("(procedure? car)", env));
    assertEquals(TRUE, eval("(procedure? procedure?)", env));
    assertEquals(TRUE, eval("(procedure? (lambda (x) x))", env));
    assertEquals(TRUE, eval("(let ((a (lambda (x) x))) (procedure? a))", env));
    assertEquals(TRUE, eval("(letrec ((a (lambda () (procedure? a)))) (a))", env));
    assertEquals(TRUE, eval("(let () (define (hi) 1) (procedure? hi))", env));
    String[] falses = {"(procedure? 'car)", "(procedure? '(lambda (x) x))",
        "(let ((a 1)) (let ((a (lambda () (procedure? a)))) (a)))", "(procedure? 'and)", "(procedure? 'let)",
        "(procedure? 'quasiquote)", "(procedure? 'cond)", "(procedure? 'do)", "(procedure? 'set!)", "(procedure? \"hi\")",
        "(procedure? '(1 2))", "(procedure? #(1 2))", "(procedure? {})", "(procedure? (find {1 2 3 4} 1))", "(procedure? [1 2 3])"};
    assertAllEqual(FALSE, falses, env);
    try {
      eval("(procedure? begin)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("begin: bad syntax in form: begin", e.getMessage());
    }
    try {
      eval("(procedure? lambda)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("lambda: bad syntax in form: lambda", e.getMessage());
    }
    try {
      eval("(procedure? and)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("and: bad syntax in form: and", e.getMessage());
    }
    try {
      eval("(procedure? let)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("let: bad syntax in form: let", e.getMessage());
    }
    try {
      eval("(procedure? quasiquote)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("quasiquote: bad syntax in form: quasiquote", e.getMessage());
    }
    try {
      eval("(procedure? cond)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("cond: bad syntax in form: cond", e.getMessage());
    }
    try {
      eval("(procedure? do)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("do: bad syntax in form: do", e.getMessage());
    }
    try {
      eval("(procedure? set!)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("set!: bad syntax in form: set!", e.getMessage());
    }
    try {
      eval("(procedure?)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("procedure?: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 0)", e.getMessage());
    }
    try {
      eval("(procedure? abs car)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("procedure?: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 2)", e.getMessage());
    }
  }
}
