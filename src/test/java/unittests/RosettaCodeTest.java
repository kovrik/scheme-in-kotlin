package unittests;

import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.procedures.io.Display;
import core.procedures.io.Newline;
import core.reader.IReader;
import core.reader.Reader;
import core.scm.SCMCons;
import core.scm.SCMSymbol;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.Calendar;

import static core.scm.SCMCons.list;
import static org.junit.Assert.assertEquals;

public class RosettaCodeTest {

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

  @Test
  public void testEvalGroupNumbers() {
    String group = "(let loop ((numbers '(3 -2 1 6 -5))" +
        "           (nonneg '())" +
        "           (neg '()))" +
        "  (cond ((null? numbers) (list nonneg neg))" +
        "        ((>= (car numbers) 0)" +
        "         (loop (cdr numbers)" +
        "               (cons (car numbers) nonneg)" +
        "               neg))" +
        "        ((< (car numbers) 0)" +
        "         (loop (cdr numbers)" +
        "               nonneg" +
        "               (cons (car numbers) neg)))))";

    assertEquals(list(list(6L, 1L, 3L), list(-5L, -2L)), eval(group, env));
  }

  @Test
  public void testFlattenList() {

    String flatten = "(define (flatten x)" +
        "    (cond ((null? x) '())" +
        "          ((not (pair? x)) (list x))" +
        "          (else (append (flatten (car x))" +
        "                        (flatten (cdr x))))))";
    eval(flatten, env);
    assertEquals(list(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
        eval("(flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))", env));
  }

  @Test
  public void testQuickSort() {

    String splitby = "(define (split-by l p k)" +
        "  (let loop ((low '())" +
        "             (high '())" +
        "             (l l))" +
        "    (cond ((null? l)" +
        "           (k low high))" +
        "          ((p (car l))" +
        "           (loop low (cons (car l) high) (cdr l)))" +
        "          (else" +
        "           (loop (cons (car l) low) high (cdr l))))))";

    String quick = "(define (quicksort l gt?)" +
        "  (if (null? l)" +
        "      '()" +
        "      (split-by (cdr l) " +
        "                (lambda (x) (gt? x (car l)))" +
        "                (lambda (low high)" +
        "                  (append (quicksort low gt?)" +
        "                          (list (car l))" +
        "                          (quicksort high gt?))))))";

    eval(splitby, env);
    eval(quick, env);
    assertEquals(list(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L),
        eval("(quicksort '(1 3 5 7 9 8 6 4 2) >)", env));
  }

  @Test
  public void testGnomeSort() {

    String gnome = "(define (gnome-sort-compar in-order input-list)" +
        "  (let gnome ((p (list (car input-list)))" +
        "              (n (cdr input-list)))" +
        "    (if (null? n)" +
        "        p" +
        "        (let ((prev-pot (car p))" +
        "              (next-pot (car n)))" +
        "          (if (in-order next-pot prev-pot)" +
        "              (gnome (cons next-pot p) (cdr n))" +
        "              (if (null? (cdr p))" +
        "                  (gnome (list next-pot) (cons prev-pot (cdr n)))" +
        "                  (gnome (cdr p) (cons next-pot (cons prev-pot (cdr n))))))))))";
    eval(gnome, env);

    String test = "(gnome-sort-compar <= '(98 36 2 78 5 81 32 90 73 21 94 28 53 25 10 99))";
    SCMCons sorted = list(2L, 5L, 10L, 21L, 25L, 28L, 32L, 36L, 53L, 73L, 78L, 81L, 90L, 94L, 98L, 99L);
    assertEquals(sorted, eval(test, env));
  }

  @Test
  public void testHailstoneSeq() {

    String hailstone = "(define (hailstone n)" +
        "  (if (= n 1) '(1)" +
        "  (cons n (hailstone (if (even? n) (/ n 2) (+ 1 (* 3 n)))))))";

    String hailstoneLength = "(define (hailstone-length n)" +
        "  (let aux ((n n) (r 1)) (if (= n 1) r" +
        "  (aux (if (even? n) (/ n 2) (+ 1 (* 3 n))) (+ r 1)))))";

    String hailstoneMax = "(define (hailstone-max a b)" +
        "  (let aux ((i a) (j 0) (k 0))" +
        "    (if (> i b) (list j k)" +
        "      (let ((h (hailstone-length i)))" +
        "        (if (> h k) (aux (+ i 1) i h) (aux (+ i 1) j k))))))";

    eval(hailstone, env);
    eval(hailstoneLength, env);
    eval(hailstoneMax, env);

    SCMCons seq = list(27L, 82L, 41L, 124L, 62L, 31L, 94L, 47L, 142L, 71L, 214L, 107L, 322L, 161L, 484L,
        242L, 121L, 364L, 182L, 91L, 274L, 137L, 412L, 206L, 103L, 310L, 155L, 466L, 233L,
        700L, 350L, 175L, 526L, 263L, 790L, 395L, 1186L, 593L, 1780L, 890L, 445L, 1336L,
        668L, 334L, 167L, 502L, 251L, 754L, 377L, 1132L, 566L, 283L, 850L, 425L, 1276L,
        638L, 319L, 958L, 479L, 1438L, 719L, 2158L, 1079L, 3238L, 1619L, 4858L, 2429L,
        7288L, 3644L, 1822L, 911L, 2734L, 1367L, 4102L, 2051L, 6154L, 3077L, 9232L, 4616L,
        2308L, 1154L, 577L, 1732L, 866L, 433L, 1300L, 650L, 325L, 976L, 488L, 244L, 122L,
        61L, 184L, 92L, 46L, 23L, 70L, 35L, 106L, 53L, 160L, 80L, 40L, 20L, 10L, 5L, 16L,
        8L, 4L, 2L, 1L);
    assertEquals(seq, eval("(hailstone 27)", env));
    assertEquals(112L, eval("(hailstone-length 27)", env));
  }

  @Test
  public void testEvalHornersRule() {
    String horner = "(define (horner lst x)" +
        "  (define (*horner lst x acc)" +
        "    (if (null? lst)" +
        "        acc" +
        "        (*horner (cdr lst) x (+ (* acc x) (car lst)))))" +
        "  (*horner (reverse lst) x 0))";
    eval(horner, env);
    assertEquals(128L, eval("(horner '(-19 7 -4 6) 3)", env));
  }

  @Test
  public void testDayOfWeek() {

    String dayOfWeek = "(define (day-of-week year month day)" +
        "  (if (< month 3)" +
        "    (begin" +
        "      (set! month (+ month 12))" +
        "      (set! year (- year 1)))" +
        "  #f)" +
        "(+ 1 " +
        "   (remainder (+ 5 day (quotient (* (+ 1 month) 13) 5) " +
        "                 year (quotient year 4) (* (quotient year 100) 6) (quotient year 400)) " +
        "              7)))";
    eval(dayOfWeek, env);
    assertEquals(3L, eval("(day-of-week 2016 6 29)", env));

    // test Today
    Calendar calendar = Calendar.getInstance();
    Integer dw = calendar.get(Calendar.DAY_OF_WEEK) - 1;
    if (dw == 0) {
      dw = 7;
    }
    int y = calendar.get(Calendar.YEAR);
    int m = calendar.get(Calendar.MONTH) + 1;
    int d = calendar.get(Calendar.DAY_OF_MONTH);
    String todaySexp = "(day-of-week %s %s %s)";
    assertEquals(dw.longValue(), eval(String.format(todaySexp, y, m, d), env));
  }

  @Test
  public void testEvalHanoi() {

    PrintStream old = System.out;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    System.setOut(new PrintStream(baos));

    IEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (String proc : tempEnv.getLibraryProcedures()) {
      eval(proc, tempEnv);
    }
    tempEnv.put(new SCMSymbol("display"), new Display(System.out));
    tempEnv.put(new SCMSymbol("newline"), new Newline(System.out));

    String hanoi = "(define (hanoi n a b c) (if (> n 0) (begin (hanoi (- n 1) a c b) (display \"Move disk from pole \") (display a) (display \" to pole \") (display b) (newline) (hanoi (- n 1) c b a)) #t))";
    eval(hanoi, tempEnv);
    eval("(hanoi 4 1 2 3)", tempEnv);

    String solution = "Move disk from pole 1 to pole 3\n" +
        "Move disk from pole 1 to pole 2\n" +
        "Move disk from pole 3 to pole 2\n" +
        "Move disk from pole 1 to pole 3\n" +
        "Move disk from pole 2 to pole 1\n" +
        "Move disk from pole 2 to pole 3\n" +
        "Move disk from pole 1 to pole 3\n" +
        "Move disk from pole 1 to pole 2\n" +
        "Move disk from pole 3 to pole 2\n" +
        "Move disk from pole 3 to pole 1\n" +
        "Move disk from pole 2 to pole 1\n" +
        "Move disk from pole 3 to pole 2\n" +
        "Move disk from pole 1 to pole 3\n" +
        "Move disk from pole 1 to pole 2\n" +
        "Move disk from pole 3 to pole 2";

    assertEquals(solution, baos.toString().trim());
    System.setOut(old);
  }

  @Test
  public void testIntegerRoots() {

    String integerRoots = "(define (root a b)" +
        "  (define // quotient)" +
        "  (define (y a a1 b c d e)" +
        "    (if (or (= c d) (= c e))" +
        "        (min d e)" +
        "      (y a a1 b d e (// (+ (* a1 e)" + // <--- y is recursive. TCO
        "                           (// b (expt e a1))) a))))" +
        "  (if (< b 2)" +
        "      b" +
        "    (let* ((a1 (- a 1))" +
        "           (c 1)" +
        "           (d (// (+ (* a1 c) (// b (expt c a1))) a))" +
        "           (e (// (+ (* a1 d) (// b (expt d a1))) a)))" +
        "      (y a a1 b c d e))))";

    IEnvironment tempEnv = new DefaultEnvironment();
    eval(integerRoots, tempEnv);
    // TODO StackOverflow
//    assertEquals(2.0, eval("(root 3 (* 2 (expt 1000 2000)))", tempEnv));
  }

  @Test
  public void testEvalQuine() {

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    PrintStream old = System.out;
    System.setOut(new PrintStream(baos));

    IEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (String proc : tempEnv.getLibraryProcedures()) {
      eval(proc, tempEnv);
    }
    tempEnv.put(new SCMSymbol("display"), new Display(System.out));

    String quine = "((lambda (s) (display (list s (list (quote quote) s))))" +
        " (quote (lambda (s) (display (list s (list (quote quote) s))))))";
    eval(quine, tempEnv);
    assertEquals(quine, baos.toString().trim());

    System.setOut(old);
  }
}
