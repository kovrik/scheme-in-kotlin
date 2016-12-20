package unittests;

import core.Repl;
import core.exceptions.ReentrantContinuationException;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.SCMOutputPort;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static core.scm.SCMBoolean.FALSE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class ContinuationsTest extends AbstractTest {

  @Test
  public void testListAdd() {
    String listadd = "(define (lstadd1 lst)" +
                     "  (call/cc (lambda (exit)" +
                     "    (let loop ((lst lst))" +
                     "       (cond ((pair? lst) (cons (add1 (car lst)) (loop (cdr lst))))" +
                     "             ((null? lst) '())" +
                     "             (else (exit #f)))))))";
    eval(listadd, env);
    assertEquals(SCMCons.list(2L, 3L, 4L), eval("(lstadd1 '(1 2 3))", env));
    assertEquals(FALSE, eval("(lstadd1 '(1 2 . 3))", env));
  }

  @Test
  public void testCC() {
    String cc = "(define (cc) (call-with-current-continuation (lambda (cc) (cc cc))))";
    eval(cc, env);
    assertEquals(SCMClass.CONTINUATION, eval("(class-of (cc))", env));
    try {
      eval("((cc) display)", env);
      fail();
    } catch (ReentrantContinuationException e) {
      // success
    }
  }

  @Test
  public void testYingYang() {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    SCMOutputPort old = Repl.getCurrentOutputPort();
    Repl.setCurrentOutputPort(new SCMOutputPort(new PrintStream(baos)));

    String yingyang = "(let* ((yin  ((lambda (cc) (display #\\@) cc) (call-with-current-continuation (lambda (c) c))))" +
                      "       (yang ((lambda (cc) (display #\\*) cc) (call-with-current-continuation (lambda (c) c)))))" +
                      "    (yin yang))";
    try {
      eval(yingyang, env);
      fail();
    } catch (ReentrantContinuationException ex) {
      assertEquals("@*", baos.toString().trim());
    }

    Repl.setCurrentOutputPort(old);
  }

  @Test
  public void testWikiExample() {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    SCMOutputPort old = Repl.getCurrentOutputPort();
    Repl.setCurrentOutputPort(new SCMOutputPort(new PrintStream(baos)));

    String example = "(define (f return)" +
                     "  (return 2)" +
                     "  3)";

    eval(example, env);
    eval("(display (f (lambda (x) x))) ; displays 3", env);
    eval("(display (call-with-current-continuation f)) ; displays 2", env);
    assertEquals("32", baos.toString().trim());

    Repl.setCurrentOutputPort(old);
  }

  @Test
  public void testContinuationExample() {
    String example = "(let ((cont #f))" +
                     "  (letrec ((x (call-with-current-continuation (lambda (c) (set! cont c) 0)))" +
                     "           (y (call-with-current-continuation (lambda (c) (set! cont c) 0))))" +
                     "    (if cont" +
                     "        (let ((c cont))" +
                     "          (set! cont #f)" +
                     "          (set! x 1)" +
                     "          (set! y 1)" +
                     "          (c 0))" +
                     "        (+ x y))))";
    try {
      eval(example, env);
      fail();
    } catch (ReentrantContinuationException ex) {
      // success
    }
  }
}
