package core.environment;

import core.procedures.characters.CharComparison;
import core.procedures.characters.CharProc;
import core.procedures.cons.*;
import core.procedures.delayed.Force;
import core.procedures.equivalence.Eq;
import core.procedures.equivalence.Equal;
import core.procedures.equivalence.Eqv;
import core.procedures.io.Display;
import core.procedures.io.Newline;
import core.procedures.lists.AssocProc;
import core.procedures.lists.Length;
import core.procedures.lists.MemberProc;
import core.procedures.math.*;
import core.procedures.strings.*;
import core.procedures.symbols.StringToSymbol;
import core.procedures.symbols.SymbolToString;
import core.procedures.system.ClassOf;
import core.procedures.system.Exit;
import core.procedures.vectors.*;
import core.scm.SCMBoolean;
import core.scm.SCMSymbol;
import core.scm.specialforms.ISpecialForm;
import core.scm.specialforms.SCMSpecialForm;

import java.util.ArrayList;
import java.util.List;

public final class DefaultEnvironment extends Environment {

  private static final List<String> procs = new ArrayList<>();
  static {
    procs.add("(define (promise?   o) (eq? (class-of (delay 1)) (class-of o)))");
    procs.add("(define (char?      o) (eq? (class-of #\\A) (class-of o)))");
    procs.add("(define (string?    o) (eq? (class-of \"str\") (class-of o)))");
    procs.add("(define (vector?    o) (eq? (class-of #()) (class-of o)))");
    procs.add("(define (symbol?    o) (eq? (class-of 'sym) (class-of o)))");
    procs.add("(define (boolean?   o) (eq? (class-of #t) (class-of o)))");
    procs.add("(define (procedure? o) (eq? (class-of (lambda () n)) (class-of o)))");
    procs.add("(define (number?    o) (if  (member (class-of o) (list (class-of 1) (class-of 1.5))) #t #f))))");
    procs.add("(define (null?      o) (eq? (class-of '()) (class-of o)))");
    procs.add("(define (pair?      o) (eq? (class-of (cons 1 2)) (class-of o)))");

    procs.add("(define (list . elements) elements)");
    procs.add("(define empty? null?)");

    // Miscellaneous predicates
    procs.add("(define (zero? n) (= n 0))");
    procs.add("(define (integer? x) (= x (round x)))");
    procs.add("(define (negative? n) (< n 0))");
    procs.add("(define (positive? n) (> n 0))");
    procs.add("(define (even? n) (= 0 (remainder n 2)))");
    procs.add("(define (odd? n) (not (even? n)))");
//    procs.add("(define (force p) (if (promise? p) (p) p))");

    // simple map
    procs.add("(define (map proc lis)" +
              "   (cond ((null? lis) '())" +
              "         ((pair? lis) (cons (proc (car lis))" +
              "                            (map proc (cdr lis))))" +
              "          (else (error \"Not a proper list!\"))))");

    // SRFI-1 Reference Map implementation
//    procs.add("(define map map-in-order)");
//    procs.add("(define (map f lis1 . lists)" +
//              "  (if (pair? lists)" +
//              "      (let recur ((lists (cons lis1 lists)))" +
//              "        (receive (cars cdrs) (%cars+cdrs lists)" +
//              "          (if (pair? cars)" +
//              "              (let ((x (apply f cars)))" +       // ; Do head first
//              "                (cons x (recur cdrs))) '())))" + // ; then tail
//                      ;; Fast path. +
//              "      (let recur ((lis lis1))" +
//              "        (if (null? lis) lis" +
//              "            (let ((tail (cdr lis))" +
//              "                  (x (f (car lis))))" +     // ; Do head first,
//              "              (cons x (recur tail)))))))"); // ; then tail

  }

  @Override
  public List<String> getLibraryProcedures() {
    return procs;
  }

  public DefaultEnvironment() {

    super(null);

    /* Special Forms */
    for (ISpecialForm specialForm : SCMSpecialForm.values()) {
      put(new SCMSymbol(specialForm.toString()), specialForm);
    }

    /* System */
    put(new SCMSymbol("exit"), new Exit());
    put(new SCMSymbol("class-of"), new ClassOf());

    /* Boolean */
    put(SCMBoolean.TRUE,  SCMBoolean.TRUE);
    put(SCMBoolean.FALSE, SCMBoolean.FALSE);

    /* Delayed */
    put(new SCMSymbol("force"), new Force());

    /* Math */
    put(new SCMSymbol("not"), new Negation());
    put(new SCMSymbol("+"),    new Addition());
    put(new SCMSymbol("-"),    new Subtraction());
    put(new SCMSymbol("*"),    new Multiplication());
    put(new SCMSymbol("/"),    new Division());
    put(new SCMSymbol("abs"),  new Abs());
    put(new SCMSymbol("sqrt"), new Sqrt());
    put(new SCMSymbol("expt"), new Expt());
    put(new SCMSymbol("modulo"), new Modulo());
    put(new SCMSymbol("remainder"), new Remainder());
    put(new SCMSymbol("quotient"), new Quotient());

    put(new SCMSymbol("round"),    new Round());
    put(new SCMSymbol("floor"),    new Floor());
    put(new SCMSymbol("ceiling"),  new Ceiling());
    put(new SCMSymbol("truncate"), new Truncate());
    put(new SCMSymbol("max"),      new Max());
    put(new SCMSymbol("min"),      new Min());
    put(new SCMSymbol("gcd"),      new GCD());
    put(new SCMSymbol("lcm"),      new LCM());

    /* Comparison & Equality */
    put(new SCMSymbol("="),  new NumericalComparison(NumericalComparison.Type.EQUAL));
    put(new SCMSymbol("<"),  new NumericalComparison(NumericalComparison.Type.LESS));
    put(new SCMSymbol("<="), new NumericalComparison(NumericalComparison.Type.LESS_EQUAL));
    put(new SCMSymbol(">"),  new NumericalComparison(NumericalComparison.Type.GREATER));
    put(new SCMSymbol(">="), new NumericalComparison(NumericalComparison.Type.GREATER_EQUAL));

    put(new SCMSymbol("eq?"),    new Eq());
    put(new SCMSymbol("eqv?"),   new Eqv());
    put(new SCMSymbol("equal?"), new Equal());

    /* Strings */
    put(new SCMSymbol("string-length"), new StringLength());
    put(new SCMSymbol("string-copy"), new StringCopy());
    put(new SCMSymbol("string"), new StringProc());
    put(new SCMSymbol("substring"), new Substring());
    put(new SCMSymbol("string-append"), new StringAppend());
    put(new SCMSymbol("string-fill!"), new StringFill());
    put(new SCMSymbol("make-string"), new MakeString());
    put(new SCMSymbol("list->string"), new ListToString());
    put(new SCMSymbol("number->string"), new NumberToString());
    put(new SCMSymbol("string->number"), new StringToNumber());
    put(new SCMSymbol("string->list"), new StringToList());
    put(new SCMSymbol("string-ref"), new StringRef());
    put(new SCMSymbol("string-set!"), new StringSet());
    put(new SCMSymbol("string>?"), StringComparison.STRING_GR);
    put(new SCMSymbol("string-ci>?"), StringComparison.STRING_GR_CI);
    put(new SCMSymbol("string>=?"), StringComparison.STRING_GR_OR_EQ);
    put(new SCMSymbol("string-ci>=?"), StringComparison.STRING_GR_OR_EQ_CI);
    put(new SCMSymbol("string<?"), StringComparison.STRING_LE);
    put(new SCMSymbol("string-ci<?"), StringComparison.STRING_LE_CI);
    put(new SCMSymbol("string<=?"), StringComparison.STRING_LE_OR_EQ);
    put(new SCMSymbol("string-ci<=?"), StringComparison.STRING_LE_OR_EQ_CI);
    put(new SCMSymbol("string=?"), StringComparison.STRING_EQ);
    put(new SCMSymbol("string-ci=?"), StringComparison.STRING_EQ_CI);

    /* Characters */
    put(new SCMSymbol("char-whitespace?"), CharProc.CHAR_WHITESPACE);
    put(new SCMSymbol("char-alphabetic?"), CharProc.CHAR_ALPHABETIC);
    put(new SCMSymbol("char-upper-case?"), CharProc.CHAR_UPPER_CASE);
    put(new SCMSymbol("char-lower-case?"), CharProc.CHAR_LOWER_CASE);
    put(new SCMSymbol("char-numeric?"), CharProc.CHAR_NUMERIC);
    put(new SCMSymbol("char->integer"), CharProc.CHAR_TO_INTEGER);
    put(new SCMSymbol("integer->char"), CharProc.INTEGER_TO_CHAR);
    put(new SCMSymbol("char-upcase"), CharProc.CHAR_UPCASE);
    put(new SCMSymbol("char-downcase"), CharProc.CHAR_DOWNCASE);
    put(new SCMSymbol("char>?"), CharComparison.CHAR_GR);
    put(new SCMSymbol("char-ci>?"), CharComparison.CHAR_GR_CI);
    put(new SCMSymbol("char>=?"), CharComparison.CHAR_GR_OR_EQ);
    put(new SCMSymbol("char-ci>=?"), CharComparison.CHAR_GR_OR_EQ_CI);
    put(new SCMSymbol("char<?"), CharComparison.CHAR_LE);
    put(new SCMSymbol("char-ci<?"), CharComparison.CHAR_LE_CI);
    put(new SCMSymbol("char<=?"), CharComparison.CHAR_LE_OR_EQ);
    put(new SCMSymbol("char-ci<=?"), CharComparison.CHAR_LE_OR_EQ_CI);
    put(new SCMSymbol("char=?"), CharComparison.CHAR_EQ);
    put(new SCMSymbol("char-ci=?"), CharComparison.CHAR_EQ_CI);

    put(new SCMSymbol("display"), new Display(System.out));
    put(new SCMSymbol("newline"), new Newline(System.out));

    /* Lists */
    put(new SCMSymbol("length"), new Length());
    put(new SCMSymbol("member"), new MemberProc("member", new Equal()));
    put(new SCMSymbol("memq"),   new MemberProc("memq",   new Eq()));
    put(new SCMSymbol("memv"),   new MemberProc("memv",   new Eqv()));
    put(new SCMSymbol("assoc"),  new AssocProc("assoc",   new Equal()));
    put(new SCMSymbol("assq"),   new AssocProc("assq",    new Eq()));
    put(new SCMSymbol("assv"),   new AssocProc("assv",    new Eqv()));

    /* Symbols */
    put(new SCMSymbol("symbol->string"), new SymbolToString());
    put(new SCMSymbol("string->symbol"), new StringToSymbol());

    /* Vectors */
    put(new SCMSymbol("make-vector"), new MakeVector());
    put(new SCMSymbol("vector"), new Vector());
    put(new SCMSymbol("vector-length"), new VectorLength());
    put(new SCMSymbol("vector-ref"), new VectorRef());
    put(new SCMSymbol("vector-set!"), new VectorSet());
    put(new SCMSymbol("list->vector"), new ListToVector());
    put(new SCMSymbol("vector->list"), new VectorToList());
    put(new SCMSymbol("vector-fill!"), new VectorFill());

    /* Cons */
    put(new SCMSymbol("list?"),  new IsList());
    put(new SCMSymbol("cons"),   new ConsProc());
    put(new SCMSymbol("car"),    new Car());
    put(new SCMSymbol("cdr"),    new Cdr());
    put(new SCMSymbol("set-car!"), new SetCar());
    put(new SCMSymbol("set-cdr!"), new SetCdr());
    put(new SCMSymbol("append"), new Append());
    put(new SCMSymbol("reverse"), new Reverse());
    put(new SCMSymbol("list-tail"), new ListTail());
    put(new SCMSymbol("list-ref"), new ListRef());

    /* Functional */
    // TODO
//    put(new SCMSymbol("map"), new MapProc());
  }
}
