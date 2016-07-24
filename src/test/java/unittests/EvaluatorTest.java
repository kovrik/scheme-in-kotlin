package unittests;

import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.procedures.io.Display;
import core.reader.IReader;
import core.reader.Reader;
import core.scm.SCMSymbol;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static org.junit.Assert.*;

public class EvaluatorTest {

  private final IReader reader = new Reader();
  private final IEvaluator eval = new Evaluator();
  private final DefaultEnvironment env = new DefaultEnvironment();
  {
    /* Eval lib procedures */
    for (String proc : env.getLibraryProcedures()) {
      eval(proc, env);
    }
  }

  /* Helper method */
  private Object eval(String sexp, IEnvironment env) {
    return eval.eval(reader.read(sexp), env);
  }

  @Before
  public void setUp() throws Exception {
    // TODO Create new environment for each test?
  }

  @Test
  public void testEvalLocalState() {
    IEnvironment lenv = new DefaultEnvironment();
    eval("(define (make-withdraw balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) \"Insufficient funds\")))", lenv);
    eval("(define W1 (make-withdraw 100))", lenv);
    eval("(define W2 (make-withdraw 100))", lenv);
    assertEquals(50L, eval("(W1 50)", lenv));
    assertEquals(30L, eval("(W2 70)", lenv));
    assertEquals("Insufficient funds", eval("(W2 40)", lenv));
    assertEquals(10L, eval("(W1 40)", lenv));

    eval("(define a 999)", lenv);
    assertEquals(5L, eval("(begin (define a 10) (set! a 5) a)", lenv));
    assertEquals(20L, eval("(let ((a 5)) (set! a 10) (let () (set! a 15) (let () (+ a 5))))", lenv));

    eval("(define x 2)", lenv);
    eval("(define y 10)", lenv);
    eval("(define multiply (lambda (x y) (* x y)))", lenv);
    assertEquals(12L, eval("(+ x y)", lenv));
    assertEquals(100L, eval("(multiply y 10)", lenv));

    eval("(define x 10)", lenv);
    eval("(set! x 20)", lenv);
    assertEquals(20L, eval("x", lenv));
    eval("(define add (lambda (x y) (set! x (+ x y)) x))", lenv);
    assertEquals(110L, eval("(add 10 100)", lenv));

    eval("(define x 4)", lenv);
    eval("(define y 5)", lenv);
    assertEquals(2L, eval("(let ((x 1) (y 2)) (* x y))", lenv));
    assertEquals(20L, eval("(* x y)", lenv));
  }

  @Test
  public void testEvalNegation() {
    assertEquals(FALSE, eval("(not #t)",  env));
    assertEquals(TRUE,  eval("(not #f)",  env));
    assertEquals(TRUE,  eval("(not (= 1 2 1))", env));
    assertEquals(FALSE, eval("(not (= 1 1 1))", env));
  }

  // Equivalence
  @Test
  public void testEvalEq() {
    assertEquals(TRUE,  eval("(eq? '() '())", env));
    assertEquals(TRUE, eval("(eq? 1 1)", env));
    assertEquals(FALSE, eval("(eq? 1 2)", env));
    assertEquals(FALSE, eval("(eq? \"1\" \"1\")", env));
  }

  @Test
  public void testEvalEqv() {
    assertEquals(TRUE,  eval("(eqv? '() '())", env));
    assertEquals(TRUE,  eval("(eqv? 1 1)", env));
    assertEquals(FALSE, eval("(eqv? 1 2)", env));
    assertEquals(FALSE, eval("(eqv? \"1\" \"1\")", env));
  }

  @Test
  public void testEvalEqual() {
    assertEquals(TRUE,  eval("(equal? '() '())", env));
    assertEquals(TRUE,  eval("(equal? '(1 2 3) '( 1 2 3))", env));
    assertEquals(FALSE, eval("(equal? '(1 2 3 5) '( 1 2 3))", env));
    assertEquals(TRUE,  eval("(equal? 1 1)", env));
    assertEquals(FALSE, eval("(equal? 1 2)", env));
    assertEquals(TRUE,  eval("(equal? \"1fe\" \"1fe\")", env));
  }

  // TODO
  @Test
  public void testTCO() {
    String recursive = "(define (recursive n)" +
                       "  (if (zero? n)" +
                       "      \"DONE\"" +
                       "    (recursive (- n 1))))";
    eval(recursive, env);

    assertEquals("DONE", eval("(recursive 5)", env));
    assertEquals("DONE", eval("(recursive 489)", env));
//    assertEquals("DONE", eval("(recursive 496)", env));
//    assertEquals("DONE", eval("(recursive 1000000)", env));
  }

  @Test
  public void testEvalDisplay() {

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    PrintStream old = System.out;
    System.setOut(new PrintStream(baos));

    IEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (String proc : tempEnv.getLibraryProcedures()) {
      eval(proc, tempEnv);
    }
    tempEnv.put(new SCMSymbol("display"), new Display(System.out));

    eval("(display 123)", tempEnv);
    assertEquals("123", baos.toString().trim());
    baos.reset();

    eval("(display -123.25)", tempEnv);
    assertEquals("-123.25", baos.toString().trim());
    baos.reset();

    eval("(display \"test string\")", tempEnv);
    assertEquals("test string", baos.toString().trim());
    baos.reset();

    eval("(display '())", tempEnv);
    assertEquals("()", baos.toString().trim());
    baos.reset();

    eval("(display '(1 2 3 #\\A (1 . 2)))", tempEnv);
    assertEquals("(1 2 3 #\\A (1 . 2))", baos.toString().trim());
    baos.reset();

    eval("(display (list 1 2 3 #\\A (cons 1 2) (list 1 2 3)))", tempEnv);
    assertEquals("(1 2 3 #\\A (1 . 2) (1 2 3))", baos.toString().trim());
    baos.reset();

    eval("(display (string->list \"Hello\"))", tempEnv);
    assertEquals("(#\\H #\\e #\\l #\\l #\\o)", baos.toString().trim());
    baos.reset();

    eval("(display (cdr (cdr '(1 2 3 4 5 6))))", tempEnv);
    assertEquals("(3 4 5 6)", baos.toString().trim());
    baos.reset();

    System.setOut(old);
  }
}
