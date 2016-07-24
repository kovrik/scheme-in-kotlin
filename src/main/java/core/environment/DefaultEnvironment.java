package core.environment;

import core.functional.MapProc;
import core.procedures.characters.CharComparison;
import core.procedures.characters.CharProc;
import core.procedures.cons.*;
import core.procedures.delayed.Force;
import core.procedures.delayed.SCMPromise;
import core.procedures.equivalence.Eq;
import core.procedures.equivalence.Equal;
import core.procedures.equivalence.Eqv;
import core.procedures.io.Display;
import core.procedures.io.Newline;
import core.procedures.lists.AssocProc;
import core.procedures.lists.Length;
import core.procedures.lists.MemberProc;
import core.procedures.math.bool.Negation;
import core.procedures.math.numeric.*;
import core.procedures.strings.*;
import core.procedures.symbols.StringToSymbol;
import core.procedures.symbols.SymbolToString;
import core.procedures.system.ClassOf;
import core.procedures.system.Exit;
import core.procedures.vectors.*;
import core.scm.SCMBoolean;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.specialforms.ISpecialForm;
import core.scm.specialforms.SCMSpecialForm;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public final class DefaultEnvironment extends Environment {

  private static final List<String> procs = new ArrayList<>();
  static {
    procs.add(String.format("(define (promise?   o) (string=? \"%s\" (class-of o)))", SCMPromise.class.getName()));
    procs.add(String.format("(define (char?      o) (string=? \"%s\" (class-of o)))", Character.class.getName()));
    procs.add(String.format("(define (string?    o) (string=? \"%s\" (class-of o)))", String.class.getName()));
    procs.add(String.format("(define (vector?    o) (string=? \"%s\" (class-of o)))", SCMVector.class.getName()));
    procs.add(String.format("(define (symbol?    o) (or (string=? \"%s\" (class-of o))" +
                                                       "(string=? \"%s\" (class-of o))" + "))",
                                                       SCMSymbol.class.getName(),
                                                       SCMSpecialForm.class.getName()));

    procs.add(String.format("(define (boolean?   o) (string=? \"%s\" (class-of o)))", SCMBoolean.class.getName()));
    procs.add(String.format("(define (procedure? o) (string=? \"%s\" (class-of o)))", SCMProcedure.class.getName()));
    procs.add(String.format("(define (number?    o) (or (string=? \"%s\" (class-of o)) " +
                                                       "(string=? \"%s\" (class-of o))" +
                                                       "(string=? \"%s\" (class-of o))" +
                                                       "(string=? \"%s\" (class-of o))))",
                                                       Long.class.getName(),
                                                       Double.class.getName(),
                                                       BigInteger.class.getName(),
                                                       BigDecimal.class.getName()));

    procs.add("(define (integer? x) (= x (round x)))");

    procs.add("(define (list . elements) elements)");
    procs.add("(define empty? null?)");

    // Miscellaneous predicates
    procs.add("(define (zero? n) (= n 0))");
    procs.add("(define (negative? n) (< n 0))");
    procs.add("(define (positive? n) (> n 0))");
    procs.add("(define (even? n) (= 0 (remainder n 2)))");
    procs.add("(define (odd? n) (not (even? n)))");
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

    put(new SCMSymbol("not"), new Negation());

    /* Math */
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

    put(new SCMSymbol("force"),   new Force());
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
    put(new SCMSymbol("cons"),   new ConsProc());
    put(new SCMSymbol("car"),    new Car());
    put(new SCMSymbol("cdr"),    new Cdr());
    put(new SCMSymbol("set-car!"), new SetCar());
    put(new SCMSymbol("set-cdr!"), new SetCdr());
    put(new SCMSymbol("pair?"),  new IsPair());
    put(new SCMSymbol("null?"),  new IsNull());
    put(new SCMSymbol("list?"),  new IsList());
    put(new SCMSymbol("append"), new Append());
    put(new SCMSymbol("reverse"), new Reverse());
    put(new SCMSymbol("list-tail"), new ListTail());
    put(new SCMSymbol("list-ref"), new ListRef());

    /* Functional */
    put(new SCMSymbol("map"), new MapProc());
  }
}
