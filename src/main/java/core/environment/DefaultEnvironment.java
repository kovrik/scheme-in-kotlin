package core.environment;

import core.procedures.delayed.Force;
import core.procedures.delayed.SCMPromise;
import core.procedures.equivalence.*;
import core.procedures.io.Display;
import core.procedures.io.Newline;
import core.procedures.lists.Length;
import core.procedures.math.bool.Negation;
import core.procedures.math.numeric.*;
import core.procedures.symbols.StringToSymbol;
import core.procedures.symbols.SymbolToString;
import core.procedures.vectors.Vector;
import core.procedures.vectors.VectorLength;
import core.scm.*;
import core.scm.specialforms.SCMSpecialForm;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

public final class DefaultEnvironment extends Environment {

  private final Map<String, String> procs = new HashMap<String, String>();
  {
    procs.put("promise?",   String.format("(define (promise?   o) (string=? \"%s\" (class-of o)))", SCMPromise.class.getName()));
    procs.put("char?",      String.format("(define (char?      o) (string=? \"%s\" (class-of o)))", Character.class.getName()));
    procs.put("string?",    String.format("(define (string?    o) (string=? \"%s\" (class-of o)))", String.class.getName()));
    procs.put("vector?",    String.format("(define (vector?    o) (string=? \"%s\" (class-of o)))", SCMVector.class.getName()));
    procs.put("symbol?",    String.format("(define (symbol?    o) (string=? \"%s\" (class-of o)))", SCMSymbol.class.getName()));
    procs.put("list?",      String.format("(define (list?      o) (string=? \"%s\" (class-of o)))", SCMList.class.getName()));
    procs.put("boolean?",   String.format("(define (boolean?   o) (string=? \"%s\" (class-of o)))", SCMBoolean.class.getName()));
    procs.put("procedure?", String.format("(define (procedure? o) (string=? \"%s\" (class-of o)))", SCMProcedure.class.getName()));
    procs.put("number?",    String.format("(define (number?    o) (or (string=? \"%s\" (class-of o)) " +
                                                                     "(string=? \"%s\" (class-of o))" +
                                                                     "(string=? \"%s\" (class-of o))))",
                                                                     Long.class.getName(),
                                                                     Double.class.getName(),
                                                                     BigDecimal.class.getName()));

//    procs.put("integer?",   String.format("(define (integer?   o) (string=? \"%s\" (class-of o)))", Long.class.getName()));
    procs.put("integer?",   "(define (integer? x) (= x (round x)))");

    procs.put("list",       "(define (list . elements) elements)");
    procs.put("null?",      "(define (null?  l) (eq? l '()))");
    procs.put("empty?",     "(define empty? null?)");

    // Miscellaneous predicates
    procs.put("zero?",      "(define (zero? n) (= n 0))");
    procs.put("negative?",  "(define (negative? n) (< n 0))");
    procs.put("positive?",  "(define (positive? n) (> n 0))");
    procs.put("even?",      "(define (even? n) (= 0 (remainder n 2)))");
    procs.put("odd?",       "(define (odd? n) (not (even? n)))");
  }

  public Map<String, String> getProcs() {
    return procs;
  }

  public DefaultEnvironment() {

    super(null);

    /* Special Forms */
    for (SCMSpecialForm specialForm : SCMSpecialForm.values()) {
      put(specialForm, specialForm);
    }

    /* Boolean */
    put(SCMBoolean.TRUE,  SCMBoolean.TRUE);
    put(SCMBoolean.FALSE, SCMBoolean.FALSE);

    put(new SCMSymbol("not"), new Negation());

    /* nil */
    // TODO?
    put(new SCMSymbol("#nil"), null);

    /* math */
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

    /* Comparison & Equality */
    put(new SCMSymbol("="),  new NumericalComparison(NumericalComparison.Type.EQUAL));
    put(new SCMSymbol("<"),  new NumericalComparison(NumericalComparison.Type.LESS));
    put(new SCMSymbol("<="), new NumericalComparison(NumericalComparison.Type.LESS_EQUAL));
    put(new SCMSymbol(">"),  new NumericalComparison(NumericalComparison.Type.GREATER));
    put(new SCMSymbol(">="), new NumericalComparison(NumericalComparison.Type.GREATER_EQUAL));

    put(new SCMSymbol("eq?"),    new Eq());
    put(new SCMSymbol("eqv?"),   new Eqv());
    put(new SCMSymbol("equal?"), new Equal());

    put(new SCMSymbol("string=?"), new StringEq());
    put(new SCMSymbol("string-ci=?"), new StringEqCi());

    put(new SCMSymbol("char=?"), new CharEq());
    put(new SCMSymbol("char-ci=?"), new CharEqCi());

    put(new SCMSymbol("force"), new Force());
    put(new SCMSymbol("display"), new Display(System.out));
    put(new SCMSymbol("newline"), new Newline(System.out));

    put(new SCMSymbol("length"), new Length());

    /* Symbols */
    put(new SCMSymbol("symbol->string"), new SymbolToString());
    put(new SCMSymbol("string->symbol"), new StringToSymbol());

    /* Vectors */
    put(new SCMSymbol("vector"), new Vector());
    put(new SCMSymbol("vector-length"), new VectorLength());
  }
}
