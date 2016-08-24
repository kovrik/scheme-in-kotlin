package core.environment;

import core.procedures.AFn;
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
import core.procedures.system.ErrorProc;
import core.procedures.system.Exit;
import core.procedures.vectors.*;
import core.scm.SCMSymbol;
import core.scm.specialforms.ISpecialForm;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static core.scm.specialforms.DefineSyntax.DEFINE_SYNTAX;
import static core.scm.specialforms.Delay.DELAY;
import static core.scm.specialforms.LetRecSyntax.LETREC_SYNTAX;
import static core.scm.specialforms.LetSyntax.LET_SYNTAX;
import static core.scm.specialforms.Quasiquote.QUASIQUOTE;
import static core.scm.specialforms.Quote.QUOTE;
import static core.scm.specialforms.Set.SET;
import static core.scm.specialforms.SyntaxRules.SYNTAX_RULES;
import static core.scm.specialforms.Unquote.UNQUOTE;
import static core.scm.specialforms.UnquoteSplicing.UNQUOTE_SPLICING;

public final class DefaultEnvironment extends Environment {

  private static final AFn[] STANDARD_PROCEDURES = {
      /* System */
      new Exit(),
      new ClassOf(),
      new ErrorProc(),

      /* Delayed */
      new Force(),

      /* Math */
      new Negation(),
      new Addition(),
      new Subtraction(),
      new Multiplication(),
      new Division(),
      new Abs(),
      new Sqrt(),
      new Expt(),
      new Modulo(),
      new Remainder(),
      new Quotient(),
      new Round(),
      new Floor(),
      new Ceiling(),
      new Truncate(),
      new Max(),
      new Min(),
      new GCD(),
      new LCM(),

      /* Comparison & Equality */
      new NumericalComparison(NumericalComparison.Type.EQUAL),
      new NumericalComparison(NumericalComparison.Type.LESS),
      new NumericalComparison(NumericalComparison.Type.LESS_EQUAL),
      new NumericalComparison(NumericalComparison.Type.GREATER),
      new NumericalComparison(NumericalComparison.Type.GREATER_EQUAL),
      new Eq(),
      new Eqv(),
      new Equal(),

      /* Strings */
      new StringLength(),
      new StringCopy(),
      new StringProc(),
      new Substring(),
      new StringAppend(),
      new StringFill(),
      new MakeString(),
      new ListToString(),
      new NumberToString(),
      new StringToNumber(),
      new StringToList(),
      new StringRef(),
      new StringSet(),
      StringComparison.STRING_GR,
      StringComparison.STRING_GR_CI,
      StringComparison.STRING_GR_OR_EQ,
      StringComparison.STRING_GR_OR_EQ_CI,
      StringComparison.STRING_LE,
      StringComparison.STRING_LE_CI,
      StringComparison.STRING_LE_OR_EQ,
      StringComparison.STRING_LE_OR_EQ_CI,
      StringComparison.STRING_EQ,
      StringComparison.STRING_EQ_CI,

      /* Characters */
      CharProc.CHAR_WHITESPACE,
      CharProc.CHAR_ALPHABETIC,
      CharProc.CHAR_UPPER_CASE,
      CharProc.CHAR_LOWER_CASE,
      CharProc.CHAR_NUMERIC,
      CharProc.CHAR_TO_INTEGER,
      CharProc.INTEGER_TO_CHAR,
      CharProc.CHAR_UPCASE,
      CharProc.CHAR_DOWNCASE,
      CharComparison.CHAR_GR,
      CharComparison.CHAR_GR_CI,
      CharComparison.CHAR_GR_OR_EQ,
      CharComparison.CHAR_GR_OR_EQ_CI,
      CharComparison.CHAR_LE,
      CharComparison.CHAR_LE_CI,
      CharComparison.CHAR_LE_OR_EQ,
      CharComparison.CHAR_LE_OR_EQ_CI,
      CharComparison.CHAR_EQ,
      CharComparison.CHAR_EQ_CI,

      /* IO */
      new Display(System.out),
      new Newline(System.out),

      /* Pairs */
      new Length(),
      new MemberProc("member", new Equal()),
      new MemberProc("memq", new Eq()),
      new MemberProc("memv", new Eqv()),
      new AssocProc("assoc", new Equal()),
      new AssocProc("assq", new Eq()),
      new AssocProc("assv", new Eqv()),
      new IsList(),
      new ConsProc(),
      new Car(),
      new Cdr(),
      new SetCar(),
      new SetCdr(),
      new Append(),
      new Reverse(),
      new ListTail(),
      new ListRef(),

      /* Symbols */
      new SymbolToString(),
      new StringToSymbol(),

      /* Vectors */
      new MakeVector(),
      new Vector(),
      new VectorLength(),
      new VectorRef(),
      new VectorSet(),
      new ListToVector(),
      new VectorToList(),
      new VectorFill(),

      /* Functional */
  };

  private static final Map<String, ISpecialForm> SPECIAL_FORMS = new HashMap<>();
  static {
    /* Without TCO */
//    SPECIAL_FORMS.put(BEGIN.toString(), BEGIN);
//    SPECIAL_FORMS.put(OR.toString(), OR);
//    SPECIAL_FORMS.put(AND.toString(), AND);
//    SPECIAL_FORMS.put(CASE.toString(), CASE);
//    SPECIAL_FORMS.put(COND.toString(), COND);
//    SPECIAL_FORMS.put(LAMBDA.toString(), LAMBDA);
//    SPECIAL_FORMS.put(DO.toString(), DO);
//    SPECIAL_FORMS.put(DEFINE.toString(), DEFINE);
//    SPECIAL_FORMS.put(IF.toString(), IF);
//    SPECIAL_FORMS.put(LET.toString(), LET);
//    SPECIAL_FORMS.put(LETSEQ.toString(), LETSEQ);
//    SPECIAL_FORMS.put(LETREC.toString(), LETREC);

    SPECIAL_FORMS.put(DELAY.toString(), DELAY);
    SPECIAL_FORMS.put(QUOTE.toString(), QUOTE);
    SPECIAL_FORMS.put(SET.toString(), SET);
    SPECIAL_FORMS.put(QUASIQUOTE.toString(), QUASIQUOTE);
    SPECIAL_FORMS.put(UNQUOTE.toString(), UNQUOTE);
    SPECIAL_FORMS.put(UNQUOTE_SPLICING.toString(), UNQUOTE_SPLICING);

    // TODO
    SPECIAL_FORMS.put(DEFINE_SYNTAX.toString(), DEFINE_SYNTAX);
    SPECIAL_FORMS.put(LET_SYNTAX.toString(), LET_SYNTAX);
    SPECIAL_FORMS.put(LETREC_SYNTAX.toString(), LETREC_SYNTAX);
    SPECIAL_FORMS.put(SYNTAX_RULES.toString(), SYNTAX_RULES);

    /* With TCO */
    SPECIAL_FORMS.put(core.scm.specialforms.tco.If.IF.toString(), core.scm.specialforms.tco.If.IF);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.Begin.BEGIN.toString(), core.scm.specialforms.tco.Begin.BEGIN);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.And.AND.toString(), core.scm.specialforms.tco.And.AND);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.Or.OR.toString(), core.scm.specialforms.tco.Or.OR);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.Lambda.LAMBDA.toString(), core.scm.specialforms.tco.Lambda.LAMBDA);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.Define.DEFINE.toString(), core.scm.specialforms.tco.Define.DEFINE);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.Let.LET.toString(), core.scm.specialforms.tco.Let.LET);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.LetRec.LETREC.toString(), core.scm.specialforms.tco.LetRec.LETREC);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.LetSeq.LETSEQ.toString(), core.scm.specialforms.tco.LetSeq.LETSEQ);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.Do.DO.toString(), core.scm.specialforms.tco.Do.DO);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.Case.CASE.toString(), core.scm.specialforms.tco.Case.CASE);
    SPECIAL_FORMS.put(core.scm.specialforms.tco.Cond.COND.toString(), core.scm.specialforms.tco.Cond.COND);
  }

  private static final List<String> LIBRARY_PROCEDURES = new ArrayList<>();
  static {
    LIBRARY_PROCEDURES.add("(define (promise?   o) (eq? (class-of (delay 1)) (class-of o)))");
    LIBRARY_PROCEDURES.add("(define (char?      o) (eq? (class-of #\\A) (class-of o)))");
    LIBRARY_PROCEDURES.add("(define (string?    o) (eq? (class-of \"str\") (class-of o)))");
    LIBRARY_PROCEDURES.add("(define (vector?    o) (eq? (class-of #()) (class-of o)))");
    LIBRARY_PROCEDURES.add("(define (symbol?    o) (eq? (class-of 'sym) (class-of o)))");
    LIBRARY_PROCEDURES.add("(define (boolean?   o) (eq? (class-of #t) (class-of o)))");
    LIBRARY_PROCEDURES.add("(define (procedure? o) (eq? (class-of (lambda () n)) (class-of o)))");
    LIBRARY_PROCEDURES.add("(define (number?    o) (if  (member (class-of o) (list (class-of 1) (class-of 1.5))) #t #f))))");
    LIBRARY_PROCEDURES.add("(define (null?      o) (eq? (class-of '()) (class-of o)))");
    LIBRARY_PROCEDURES.add("(define (pair?      o) (eq? (class-of (cons 1 2)) (class-of o)))");
    LIBRARY_PROCEDURES.add("(define empty? null?)");
    LIBRARY_PROCEDURES.add("(define (zero? n) (= n 0))");
    LIBRARY_PROCEDURES.add("(define (integer? x) (= x (round x)))");
    LIBRARY_PROCEDURES.add("(define (negative? n) (< n 0))");
    LIBRARY_PROCEDURES.add("(define (positive? n) (> n 0))");
    LIBRARY_PROCEDURES.add("(define (even? n) (= 0 (remainder n 2)))");
    LIBRARY_PROCEDURES.add("(define (odd? n) (not (even? n)))");

    LIBRARY_PROCEDURES.add("(define (list . elements) elements)");

    // simple map
    LIBRARY_PROCEDURES.add("(define (map proc lis)" +
                           "   (cond ((null? lis) '())" +
                           "         ((pair? lis) (cons (proc (car lis))" +
                           "                            (map proc (cdr lis))))" +
                           "          (else (error \"Not a proper list!\"))))");

    // SRFI-1 Reference Map implementation
//    LIBRARY_PROCEDURES.add("(define map map-in-order)");
//    LIBRARY_PROCEDURES.add("(define (map f lis1 . lists)" +
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
    return LIBRARY_PROCEDURES;
  }

  public DefaultEnvironment() {
    super(null);

    /* Special Forms */
    for (ISpecialForm specialForm : SPECIAL_FORMS.values()) {
      put(new SCMSymbol(specialForm.toString()), specialForm);
    }

    /* Standard Procedures */
    for (AFn proc : STANDARD_PROCEDURES) {
      put(new SCMSymbol(proc.getName()), proc);
    }
  }
}
