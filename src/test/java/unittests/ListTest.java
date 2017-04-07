package unittests;

import core.exceptions.ArityException;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.SCMMutableVector;
import core.scm.SCMSymbol;
import org.junit.Test;

import static core.scm.SCMCons.*;
import static core.scm.SCMConstant.UNSPECIFIED;
import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class ListTest extends AbstractTest {

  @Test
  public void testEvalList() {
    assertEquals(SCMClass.LIST, eval("(class-of (list 1 2 3 4 5))", env));
    assertEquals(list(1L, 2L, 3L), eval("(list 1 2 3)", env));
  }

  @Test
  public void testEvalIsList() {
    assertEquals(TRUE, eval("(list? '())", env));
    assertEquals(TRUE, eval("(list? '(1 2 3))", env));
    assertEquals(FALSE, eval("(list? #(1 2 3))", env));
    assertEquals(FALSE, eval("(list? (cons 1 2))", env));
    assertEquals(FALSE, eval("(list? 2)", env));
    assertEquals(TRUE, eval("(list? (car '((1 2 3))))", env));
    assertEquals(TRUE, eval("(list? (cdr '((1 2 3))))", env));
    assertEquals(FALSE, eval("(list? (car '((1 . 2))))", env));
    assertEquals(FALSE, eval("(list? (vector-ref #((1 2 3 . 4)) 0))", env));
    assertEquals(FALSE, eval("(list? (vector-ref #((1 . 2)) 0))", env));
  }

  @Test
  public void testEvalEmpty() {
    assertEquals(TRUE,  eval("(null?  '())", env));
    assertEquals(TRUE,  eval("(empty? '())", env));
    assertEquals(FALSE, eval("(null?  '(1 2 3))", env));
    assertEquals(FALSE, eval("(empty? '(1 2 3))", env));
    assertEquals(FALSE, eval("(null?  1)", env));
    assertEquals(FALSE, eval("(empty? 1)", env));
    assertEquals(TRUE,  eval("(null? (cdr '(1)))", env));
  }

  @Test
  public void testEvalListToVector() {
    assertEquals(new SCMMutableVector(1L, 2L, "test"), eval("(list->vector '(1 2 \"test\"))", env));
    assertEquals(new SCMMutableVector(), eval("(list->vector '())", env));
    try {
      eval("(list->vector #(1 2 3))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().startsWith("list->vector: type mismatch; (expected: List, given:"));
    }
  }

  @Test
  public void testCons() {
    assertEquals(TRUE, eval("(equal? '(1 2)   (cons 1 (cons 2 '())))", env));
    assertEquals(TRUE, eval("(equal? '(1 2 3) (cons 1 (cons 2 (cons 3 '()))))", env));
    assertEquals(TRUE, eval("(equal? '(1 2 3) (cons 1 '(2 3))))", env));
    assertEquals(TRUE, eval("(equal? '(1)     (cons 1 '())))", env));
    assertEquals(TRUE, eval("(equal? (cons 1 2) (cons 1 2)))", env));
    // check that we do not modify the original list/cons, but return new instead
    eval("(define conslist '())", env);
    eval("(cons 1 conslist)", env);
    assertEquals(TRUE, eval("(equal? '() conslist))", env));
    eval("(define conslist '(3))", env);
    eval("(cons 1 conslist)", env);
    assertEquals(TRUE, eval("(equal? '(3) conslist))", env));
  }

  @Test
  public void testIsPair() {
    assertEquals(FALSE, eval("(pair? '())", env));
    assertEquals(FALSE, eval("(pair? 1)", env));
    assertEquals(FALSE, eval("(pair? #(1 2))", env));
    assertEquals(TRUE,  eval("(pair? '(1))", env));
    assertEquals(TRUE,  eval("(pair? '(1 2))", env));
    assertEquals(TRUE,  eval("(pair? '(1 2 3))", env));
    assertEquals(TRUE,  eval("(pair? (cons 1 2))", env));
    assertEquals(TRUE,  eval("(pair? (cons 1 '()))", env));
    assertEquals(TRUE,  eval("(pair? (cons 1 (cons 2 3))))", env));
  }

  @Test
  public void testCar() {
    assertEquals(1L, eval("(car (cons 1 2))", env));
    assertEquals("test", eval("(car (cons \"test\" 2))", env));
    assertEquals(1L, eval("(car (cons 1 (cons 2 3)))", env));
    assertEquals(1L, eval("(car '(1 2 3))", env));
    assertEquals(1L, eval("(car '(1))", env));
    assertEquals(1L, eval("(car (list 1))", env));
    try {
      eval("(car '())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("car: type mismatch; (expected: Pair, given: ())", e.getMessage());
    }
    try {
      eval("(car 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("car: type mismatch; (expected: Pair, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testCdr() {
    assertEquals(2L, eval("(cdr (cons 1 2))", env));
    assertEquals("test", eval("(cdr (cons 2 \"test\"))", env));
    assertEquals(cons(2L, 3L), eval("(cdr (cons 1 (cons 2 3)))", env));
    assertEquals(list(2L, 3L), eval("(cdr '(1 2 3))", env));
    assertEquals(SCMCons.NIL, eval("(cdr '(1))", env));
    assertEquals(SCMCons.NIL, eval("(cdr (list 1))", env));
    try {
      eval("(cdr '())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("cdr: type mismatch; (expected: Pair, given: ())", e.getMessage());
    }
    try {
      eval("(cdr 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("cdr: type mismatch; (expected: Pair, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testSetCar() {
    assertEquals(UNSPECIFIED, eval("(set-car! '(1) 2)", env));
    assertEquals(3L, eval("(let ((a '(1))) (set-car! a 3) (car a)))", env));
    assertEquals("test", eval("(let ((a '(1 2 3))) (set-car! a \"test\") (car a)))", env));
    assertEquals("test", eval("(let ((a (cons 3 4))) (set-car! a \"test\") (car a)))", env));
    try {
      eval("(set-car! '() 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("set-car!: type mismatch; (expected: Pair, given: ())", e.getMessage());
    }
    try {
      eval("(set-car! 5 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("set-car!: type mismatch; (expected: Pair, given: 5)", e.getMessage());
    }
  }

  @Test
  public void testSetCdr() {
    assertEquals(UNSPECIFIED, eval("(set-cdr! '(1) 2)", env));
    assertEquals(3L, eval("(let ((a '(1))) (set-cdr! a 3) (cdr a)))", env));
    assertEquals("test", eval("(let ((a '(1))) (set-cdr! a \"test\") (cdr a)))", env));
    assertEquals(list(2L, 3L, 4L), eval("(let ((a '(1))) (set-cdr! a '(2 3 4)) (cdr a)))", env));
    assertEquals(3L, eval("(let ((a (cons 1 2))) (set-cdr! a 3) (cdr a)))", env));
    assertEquals(2L, eval("(let ((a (cons 1 2))) (set-cdr! a '(3 4 5)) (cdr (cons 1 2)))", env));
    try {
      eval("(set-cdr! '() 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("set-cdr!: type mismatch; (expected: Pair, given: ())", e.getMessage());
    }
    try {
      eval("(set-cdr! 5 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("set-cdr!: type mismatch; (expected: Pair, given: 5)", e.getMessage());
    }
  }

  @Test
  public void testAppend() {
    assertEquals("test", eval("(append '() \"test\")", env));
    assertEquals(5L, eval("(append '() 5)", env));
    assertEquals(cons(1L, 5L), eval("(append '(1) 5)", env));
    assertEquals(list(1L, 2L, 3L), eval("(append '(1) '(2 3))", env));
    assertEquals(list(1L, 2L, 2L, 3L), eval("(append '(1 2) '(2 3))", env));
    assertEquals(list(1L, 2L, 3L, 4L, 5L), eval("(append '(1) '(2) '(3 4) '(5))", env));
    assertEquals(cons(1L, 2L), eval("(append '() (cons 1 2))", env));
    assertEquals(cons(1L, cons(1L, 2L)), eval("(append '(1) (cons 1 2))", env));
    assertEquals(cons(1L, cons(1L, cons(1L, 2L))), eval("(append '(1 1) (cons 1 2))", env));
    assertEquals(NIL, eval("(append '() '() '() '())", env));
    try {
      eval("(append 1 '())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("append: type mismatch; (expected: List, given: 1)", e.getMessage());
    }
    try {
      eval("(append '() '() 5 '())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("append: type mismatch; (expected: List, given: 5)", e.getMessage());
    }
  }

  @Test
  public void testReverse() {
    assertEquals(NIL, eval("(reverse '())", env));
    assertEquals(list(1L), eval("(reverse '(1))", env));
    assertEquals(list(1L, 2L, 3L), eval("(reverse '(3 2 1))", env));
    assertEquals(list(1L, 2L, 3L), eval("(reverse (reverse '(1 2 3)))", env));
    try {
      eval("(reverse 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("reverse: type mismatch; (expected: List, given: 1)", e.getMessage());
    }
    try {
      eval("(reverse '(1 2) '(3 4))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("reverse: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 2)", e.getMessage());
    }
  }

  @Test
  public void testListTail() {
    assertEquals(list(3L, 4L), eval("(list-tail (list 1 2 3 4) 2)", env));
    assertEquals(2L, eval("(list-tail (cons 1 2) 1)", env));
    assertEquals(SCMSymbol.of("not-a-pair"), eval("(list-tail 'not-a-pair 0)", env));

    eval("(define a '(1 2 3 4))", env);
    eval("(define b (list-tail (cdr a) 2))", env);
    eval("(set-cdr! b '(33 44))", env);
    assertEquals(list(1L, 2L, 3L, 4L, 33L, 44L), eval("a", env));
    assertEquals(list(4L, 33L, 44L), eval("b", env));
    try {
      eval("(list-tail 1 2)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("list-tail: type mismatch; (expected: List, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testListRef() {
    assertEquals(1L, eval("(list-ref '(1) 0)", env));
    assertEquals(3L, eval("(list-ref '(1 2 3) 2)", env));
    assertEquals(1L, eval("(list-ref (cons 1 2) 0)", env));
    assertEquals(SCMSymbol.of("c"), eval("(list-ref (list 'a 'b 'c) 2)", env));
    assertEquals(cons(1L, 2L), eval("(list-ref '(1 2 (1 . 2)) 2)", env));
    assertEquals(list(1L, 2L), eval("(list-ref '(1 2 (1 2)) 2)", env));
    try {
      eval("(list-ref 1 2)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("list-ref: type mismatch; (expected: Pair, given: 1)", e.getMessage());
    }
    try {
      eval("(list-ref '(1 2) 2.5)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("list-ref: type mismatch; (expected: ExactNonNegativeInteger, given: 2.5)", e.getMessage());
    }
  }

  @Test
  public void testListToString() {
    assertEquals("", eval("(list->string '())", env));
    assertEquals("AB", eval("(list->string '(#\\A #\\B))", env));
    assertEquals("B", eval("(list->string (cdr '(#\\A #\\B)))", env));
    try {
      eval("(list->string (cons 1 2))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("list->string: type mismatch; (expected: List, given: (1 . 2))", e.getMessage());
    }
    try {
      eval("(list->string (list 1 2))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("list->string: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testEvalLength() {
    assertEquals(0L, eval("(length '())", env));
    assertEquals(1L, eval("(length '(1))", env));
    assertEquals(5L, eval("(length '(1 2 3 4 5))", env));
    try {
      eval("(length)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("length: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 0)", e.getMessage());
    }
    try {
      eval("(length 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("length: type mismatch; (expected: List, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testEvalMember() {
    assertEquals(FALSE, eval("(member 0 '())", env));
    assertEquals(FALSE, eval("(member 0 '(1 2 3))", env));
    assertEquals(FALSE, eval("(member \"test\" '(1 2 3))", env));

    assertEquals(list(1L, 2L, 3L), eval("(member 1 '(1 2 3))", env));
    assertEquals(list(2L, 3L), eval("(member 2 '(1 2 3))", env));
    assertEquals(list(3L), eval("(member 3 '(1 2 3))", env));
    assertEquals(list(list(SCMSymbol.of("a")), SCMSymbol.of("c")), eval("(member (list 'a) '(b (a) c))", env));
    try {
      eval("(member)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("member: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.getMessage());
    }
    try {
      eval("(member 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().startsWith("member: type mismatch; (expected: List, given:"));
    }
  }

  @Test
  public void testEvalMemq() {
    assertEquals(FALSE, eval("(memq 0 '())", env));
    assertEquals(FALSE, eval("(memq 0 '(1 2 3))", env));
    assertEquals(FALSE, eval("(memq \"test\" '(1 2 3))", env));

    assertEquals(list(1L, 2L, 3L), eval("(memq 1 '(1 2 3))", env));
    assertEquals(list(2L, 3L), eval("(memq 2 '(1 2 3))", env));
    assertEquals(list(3L), eval("(memq 3 '(1 2 3))", env));
    assertEquals(FALSE, eval("(memq (list 'a) '(b (a) c))", env));

    assertEquals(list(SCMSymbol.of("a"), SCMSymbol.of("b"), SCMSymbol.of("c")), eval("(memq 'a '(a b c))", env));
    assertEquals(list(SCMSymbol.of("b"), SCMSymbol.of("c")), eval("(memq 'b '(a b c))", env));
    assertEquals(FALSE, eval("(memq 'a '(b c d))", env));
    try {
      eval("(memq)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("memq: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.getMessage());
    }
    try {
      eval("(memq 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().startsWith("memq: type mismatch; (expected: List, given:"));
    }
  }

  @Test
  public void testEvalMemv() {
    assertEquals(FALSE, eval("(memv 0 '())", env));
    assertEquals(FALSE, eval("(memv 0 '(1 2 3))", env));
    assertEquals(FALSE, eval("(memv \"test\" '(1 2 3))", env));

    assertEquals(list(1L, 2L, 3L), eval("(memv 1 '(1 2 3))", env));
    assertEquals(list(2L, 3L), eval("(memv 2 '(1 2 3))", env));
    assertEquals(list(3L), eval("(memv 3 '(1 2 3))", env));
    assertEquals(FALSE, eval("(memv (list 'a) '(b (a) c))", env));

    assertEquals(list(SCMSymbol.of("a"), SCMSymbol.of("b"), SCMSymbol.of("c")), eval("(memv 'a '(a b c))", env));
    assertEquals(list(SCMSymbol.of("b"), SCMSymbol.of("c")), eval("(memv 'b '(a b c))", env));
    assertEquals(FALSE, eval("(memv 'a '(b c d))", env));

    assertEquals(list(101L, 102L), eval("(memv 101 '(100 101 102))", env));
    try {
      eval("(memv)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("memv: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.getMessage());
    }
    try {
      eval("(memv 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().startsWith("memv: type mismatch; (expected: List, given:"));
    }
  }

  @Test
  public void testEvalAssoc() {
    eval("(define e '((a 1) (b 2) (c 3)))", env);
    assertEquals(list((Object)list(SCMSymbol.of("a"))), eval("(assoc (list 'a) '(((a)) ((b)) ((c))))", env));
    try {
      eval("(assoc)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("assoc: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.getMessage());
    }
    try {
      eval("(assoc 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().startsWith("assoc: type mismatch; (expected: List or Map, given:"));
    }
    try {
      eval("(assoc 1 '((a 2) 3))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("assoc: wrong type argument in position 1 (expecting association list): ((a 2) 3)", e.getMessage());
    }
  }

  @Test
  public void testEvalAssq() {
    eval("(define e '((a 1) (b 2) (c 3)))", env);
    assertEquals(list(SCMSymbol.of("a"), 1L), eval("(assq 'a e)", env));
    assertEquals(list(SCMSymbol.of("b"), 2L), eval("(assq 'b e)", env));
    assertEquals(FALSE, eval("(assq 'd e)", env));
    try {
      eval("(assq)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("assq: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.getMessage());
    }
    try {
      eval("(assq 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().startsWith("assq: type mismatch; (expected: List, given:"));
    }
    try {
      eval("(assq 1 '((a 2) 3))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("assq: wrong type argument in position 1 (expecting association list): ((a 2) 3)", e.getMessage());
    }
  }

  @Test
  public void testEvalAssv() {
    assertEquals(list(5L, 7L), eval("(assv 5 '((2 3) (5 7) (11 13)))", env));
    try {
      eval("(assv)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("assv: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 0)", e.getMessage());
    }
    try {
      eval("(assv 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().startsWith("assv: type mismatch; (expected: List, given:"));
    }
    try {
      eval("(assv 1 '((a 2) 3))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("assv: wrong type argument in position 1 (expecting association list): ((a 2) 3)", e.getMessage());
    }
  }
}
