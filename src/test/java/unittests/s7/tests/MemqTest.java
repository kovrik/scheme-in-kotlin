package unittests.s7.tests;

import core.scm.SCMCons;
import core.scm.SCMSymbol;
import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMCons.cons;
import static core.scm.SCMCons.list;
import static junit.framework.TestCase.assertEquals;

public class MemqTest extends AbstractS7Test {

  @Test
  public void testMemq() {
    assertEquals(list(s("a"), s("b"), s("c")), eval("(memq 'a '(a b c))", env));
    assertEquals(list(s("a"), s("b"), s("c")), eval("(memq 'a (list 'a 'b 'c))", env));
    assertEquals(list(s("b"), s("c")), eval("(memq 'b '(a b c))", env));
    assertEquals(FALSE, eval("(memq 'a '(b c d))", env));
    assertEquals(FALSE, eval("(memq (list 'a) '(b (a) c))", env));
    assertEquals(list(s("a"), s("c"), s("a"), s("d"), s("a")), eval("(memq 'a '(b a c a d a))", env));
    assertEquals(list(FALSE, 2L), eval("(memq #f '(1 a #t \"hi\" #f 2))", env));
    assertEquals(FALSE, eval("(memq eq? (list 2 eqv? 2))", env));
    assertEquals(list(6L), eval("(memq 6 (memq 5 (memq 4 (memq 3 (memq 2 (memq 1 '(1 2 3 4 5 6)))))))", env));
    assertEquals(cons(s("a"), s("b")), eval("(memq 'a (cons 'a 'b))", env));
    assertEquals(cons(s("a"), cons(s("b"), s("c"))), eval("(memq 'a '(a b . c))", env));
    assertEquals(cons(s("b"), s("c")), eval("(memq 'b '(a b . c))", env));
    assertEquals(list(SCMCons.NIL, 3L), eval("(memq '() '(1 () 3))", env));
    assertEquals(FALSE, eval("(memq '() '(1 2))", env));
    assertEquals(list(s("a"), s("b"), s("c")), eval("(memq 'a '(c d a b c))", env));
    assertEquals(FALSE, eval("(memq 'a '(c d f b c))", env));
    assertEquals(FALSE, eval("(memq 'a '())", env));
    assertEquals(cons(s("a"), cons(s("b"), s("c"))), eval("(memq 'a '(c d a b . c))", env));
    assertEquals(FALSE, eval("(memq #f '(1 \"hi\" #t))", env));
    assertEquals(FALSE, eval("(memq '() '())", env));
    assertEquals(FALSE, eval("(memq '() (list))", env));
    assertEquals(list((Object)list()), eval("(memq '() (list '()))", env));
    assertEquals(list(cons(1L, 2L), cons(3L, 4L)), eval("(let ((x (cons 1 2))) (memq x (list x (cons 3 4))))", env));
    assertEquals(TRUE, eval("(pair? (let ((x (lambda () 1))) (memq x (list 1 2 x 3))))", env));
    assertEquals(list(s("a"), s("a"), s("a")), eval("(memq 'a '(a a a))", env));
    assertEquals(list(s("a"), s("a")), eval("(memq 'a '(b a a))", env));
    assertEquals(FALSE, eval("(memq \"hi\" '(1 \"hi\" 2))", env));
    assertEquals(list('a', 2L), eval("(memq #\\a '(1 #f #\\a 2))", env));
    try {
      eval("(memq 'c '(a b . c))", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument in position 2 (expecting list): (a b . c)", e.getMessage());
    }
    try {
      eval("(memq 'a '(c d f b . c))", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument in position 5 (expecting list): (c d f b . c)", e.getMessage());
    }
//    assertEquals(list(new Eq()), eval("(memq eq? (list 2 eqv? 1 eq?))", env));
//    assertEquals(, eval("(memq memq (list abs + memq car))", env));
  }

  private static SCMSymbol s(String str) {
    return new SCMSymbol(str);
  }
}