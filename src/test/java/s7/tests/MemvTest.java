package s7.tests;

import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMCons.*;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.fail;

public class MemvTest extends AbstractS7Test {

  @Test
  public void testMemv() {
    assertEquals(list(101L, 102L), eval("(memv 101 '(100 101 102))", env));
    assertEquals(list(101L, 102L), eval("(memv 101 (list 100 101 102))", env));
    assertEquals(list(3.4, 4.5), eval("(memv 3.4 '(1.2 2.3 3.4 4.5))", env));
    assertEquals(FALSE, eval("(memv 3.4 '(1.3 2.5 3.7 4.9))", env));
    assertEquals(FALSE, eval("(memv 1.0 '(1 2 3))", env));
    assertEquals(cons(1L, 2L), eval("(memv 1 (cons 1 2))", env));
    assertEquals(cons(s("a"), cons(s("b"), s("c"))), eval("(memv 'a '(a b . c))", env));
    assertEquals(list(s("c")), eval("(memv 'c '(a b c))", env));
    assertEquals(FALSE, eval("(memv 'asdf '(a b . c))", env));
    assertEquals(FALSE, eval("(memv 'c '(a b . c))", env));
    assertEquals(FALSE, eval("(memv ''a '('a b c))", env));
    assertEquals(FALSE, eval("(let ((x (cons 1 2))) (memv x (list (cons 1 2) (cons 3 4))))", env));
    assertEquals(list(cons(1L, 2L), cons(3L, 4L)), eval("(let ((x (cons 1 2))) (memv x (list x (cons 3 4))))", env));
    assertEquals(list(s("a"), s("a"), s("a")), eval("(memv 'a '(a a a))", env));
    assertEquals(list(s("a"), s("a")), eval("(memv 'a '(b a a))", env));
    assertEquals(FALSE, eval("(memv \"hi\" '(1 \"hi\" 2))", env));
    assertEquals(list('a', 2L), eval("(memv #\\a '(1 #f #\\a 2))", env));
    assertEquals(FALSE, eval("(memv #(1) '(1 #(1) 2))", env));
    assertEquals(list(NIL, 2L), eval("(memv '() '(1 () 2))", env));
    assertEquals(list(new SCMVector(1L, 2L, 3L), new SCMVector(1L, 2L)), eval("(let* ((x (vector 1 2 3)) (lst (list 1 \"hi\" x (vector 1 2)))) (memv x lst))", env));
    assertEquals(FALSE, eval("(let* ((x (vector 1 2 3)) (lst (list 1 \"hi\" (vector 1 2 3)))) (memv x lst))", env));

    try {
      eval("(memv 'a (list 'a 'b . 'c))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Bad syntax in form: quote", e.getMessage());
    }
    try {
      eval("(memv)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 0, expected: 2) passed to: memv", e.getMessage());
    }
    try {
      eval("(memv 'a)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 1, expected: 2) passed to: memv", e.getMessage());
    }
    try {
      eval("(memv 'a 'b)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument to `memv`! Expected: List, Actual: b", e.getMessage());
    }
  }

  private static SCMSymbol s(String str) {
    return new SCMSymbol(str);
  }
}
