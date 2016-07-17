package s7.tests;

import core.scm.SCMCons;
import core.scm.SCMSymbol;
import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
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
//    assertEquals(list(new Eq()), eval("(memq eq? (list 2 eqv? 1 eq?))", env));
    assertEquals(FALSE, eval("(memq eq? (list 2 eqv? 2))", env));
    assertEquals(list(6L), eval("(memq 6 (memq 5 (memq 4 (memq 3 (memq 2 (memq 1 '(1 2 3 4 5 6)))))))", env));
    assertEquals(cons(s("a"), s("b")), eval("(memq 'a (cons 'a 'b))", env));
    assertEquals(cons(s("a"), cons(s("b"), s("c"))), eval("(memq 'a '(a b . c))", env));
    assertEquals(cons(s("b"), s("c")), eval("(memq 'b '(a b . c))", env));
    assertEquals(list(SCMCons.NIL, 3L), eval("(memq '() '(1 () 3))", env));
    try {
      eval("(memq 'c '(a b . c))", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument in position 2 (expecting list): (a b . c)", e.getMessage());
    }
//    assertEquals(FALSE, eval("(memq '() '(1 2)) #f)
//    assertEquals(, eval("(memq 'a '(c d a b c)) '(a b c))
//    assertEquals(FALSE, eval("(memq 'a '(c d f b c)) #f)
//    assertEquals(FALSE, eval("(memq 'a '()) #f)
//    assertEquals(, eval("(memq 'a '(c d a b . c)) '(a b . c))
//    assertEquals(FALSE, eval("(memq 'a '(c d f b . c)) #f)
//    assertEquals(FALSE, eval("(memq #f '(1 \"hi\" #t)) #f)
//    assertEquals(FALSE, eval("(memq '() '()) #f)
//    assertEquals(FALSE, eval("(memq '() (list)) #f)
//    assertEquals(, eval("(memq '() (list ())) '(()))
//    assertEquals(, eval("(let ((x (cons 1 2))) (memq x (list x (cons 3 4)))) '((1 . 2) (3 . 4)))
//    assertEquals(, eval("(pair? (let ((x (lambda () 1))) (memq x (list 1 2 x 3)))) #t)
//    assertEquals(, eval("(memq memq (list abs + memq car)) (list memq car))
//    assertEquals(, eval("(memq 'a '(a a a)) '(a a a)) ;?
//    assertEquals(, eval("(memq 'a '(b a a)) '(a a))
//    assertEquals(FALSE, eval("(memq \"hi\" '(1 \"hi\" 2)) #f)
//    assertEquals(, eval("(memq #\\a '(1 #f #\\a 2)) '(#\\a 2))
//    assertEquals(, eval("(let* ((x (vector 1 2 3)) (lst (list 1 \"hi\" x (vector 1 2)))) (memq x lst)) '(#(1 2 3) #(1 2)))
//    assertEquals(FALSE, eval("(let* ((x (vector 1 2 3)) (lst (list 1 \"hi\" (vector 1 2 3)))) (memq x lst)) #f)
  }

  private static SCMSymbol s(String str) {
    return new SCMSymbol(str);
  }
}
