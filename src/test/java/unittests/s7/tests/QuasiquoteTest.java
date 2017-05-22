package unittests.s7.tests;

import core.exceptions.IllegalSyntaxException;
import core.exceptions.UndefinedIdentifierException;
import core.procedures.cons.ConsProc;
import org.junit.Test;
import unittests.AbstractTest;

import static core.scm.Cons.EMPTY;
import static core.scm.Cons.list;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class QuasiquoteTest extends AbstractTest {

  private static final String LS = System.getProperty("line.separator");

  @Test
  public void testQuasiquote() {
    assertEquals(list(1L, 2L, 3L), eval("`(1 2 3)", env));
    assertEquals(EMPTY, eval("`()", env));
    assertEquals(list(s("list"), 3L, 4L), eval("`(list ,(+ 1 2) 4)", env));
    assertEquals(list(1L, 1L, 2L, 4L), eval("`(1 ,@(list 1 2) 4)", env));
    assertEquals(list(s("a"), 3L, 4L, 5L, 6L, s("b")), eval("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", env));
    assertEquals(list(s("a"), list(s("quasiquote"), list(s("b"), list(s("unquote"), s("x")), list(s("unquote"), list(s("quote"), s("y"))), s("d"))), s("e")),
                 eval("(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))", env));
    assertEquals(list(1L, 2L, 81L, 3L, 4L), eval("`(1 2 ,(* 9 9) 3 4)", env));
    assertEquals(list(1L, 2L, 3L), eval("`(1 ,(+ 1 1) 3)", env));
    assertEquals(list(3L), eval("`(,(+ 1 2))", env));
    assertEquals(ConsProc.Companion.cons(s("a"), s("b")), eval("`(,'a . ,'b)", env));
    assertEquals(s("foo"), eval("`(,@'() . foo)", env));
    assertEquals(list(1L, 2L), eval("`(1 , 2)", env));
    assertEquals(list(1L, 2L, 3L), eval("`(1 ,@ (list 2 3))", env));
    assertEquals(list(1L), eval("`(1 ,@(list))", env));
    assertEquals(list(1L), eval("`(1 ,@'())", env));
    assertEquals(list(1L, 2L), eval("`(1 , ;a comment" + LS + "2)", env));
    assertEquals(list(1L, 1L), eval("`(,1 ,1)", env));
    assertEquals(list(1L, 1L), eval("`(,1 ,`,1)", env));
    assertEquals(list(1L, 1L), eval("`(,1 ,`,`,1)", env));
    assertEquals(s("quote"), eval("(quasiquote quote)", env));
    assertEquals(list(0L, 1L, 2L, 3L), eval("(let ((x '(1 2 3))) `(0 . ,x))", env));
    assertEquals(list(0L, list(1L, 2L, 3L)), eval("(let ((x '(1 2 3))) `(0 ,x))", env));
    assertEquals(list(1L, 2L, 3L), eval("(quasiquote (1 2 3))", env));
    assertEquals(EMPTY, eval("(quasiquote ())", env));
    assertEquals(list(s("list"), 3L, 4L), eval("(quasiquote (list ,(+ 1 2) 4))", env));
    assertEquals(list(1L, 1L, 2L, 4L), eval("(quasiquote (1 ,@(list 1 2) 4))", env));
    assertEquals(list(s("a"), 3L, 4L, 5L, 6L, s("b")), eval("(quasiquote (a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))", env));
    assertEquals(list(1L, 2L, 81L, 3L, 4L), eval("(quasiquote (1 2 ,(* 9 9) 3 4))", env));
    assertEquals(list(1L, 2L, 3L), eval("(quasiquote (1 ,(+ 1 1) 3))", env));
    assertEquals(list(3L), eval("(quasiquote (,(+ 1 2)))", env));
    assertEquals(s("foo"), eval("(quasiquote (,@'() . foo))", env));
    assertEquals(list(1L, 2L), eval("(quasiquote (1 , 2))", env));
    assertEquals(list(1L, 1L), eval("(quasiquote (,1 ,1))", env));
    assertEquals(list(1L, 1L), eval("(quasiquote (,1 ,(quasiquote ,1)))", env));
    assertEquals(list(1L, 1L), eval("(quasiquote (,1 ,(quasiquote ,(quasiquote ,1))))", env));
    assertEquals(list(s("+")), eval("(let ((x '())) `(+ ,@x)) '(+)", env));
    assertEquals(1L, eval("`(,@1)", env));
    assertEquals("String", eval("`(,@\"String\")", env));
    try {
      eval("`(1 , %(list 2 3))", env);
      fail();
    } catch (UndefinedIdentifierException e) {
      // expected
    }
    // FIXME `#(unquote 1)
    String[] illegals = {"`,@#(list 1 2)", ",(1 (unquote 1 2 3))", "`((unquote (+ 1 2) (+3 4)))",};
    for (String s : illegals) {
      try {
        eval(s, env);
        fail(s);
      } catch (IllegalSyntaxException e) {
        // expected
      }
    }
  }
}
