package core.environment;

import core.procedures.AFn;
import core.procedures.characters.CharComparison;
import core.procedures.characters.CharProc;
import core.procedures.cons.*;
import core.procedures.delayed.Force;
import core.procedures.equivalence.Eq;
import core.procedures.equivalence.Equal;
import core.procedures.equivalence.Eqv;
import core.procedures.functional.Apply;
import core.procedures.functional.ForEach;
import core.procedures.functional.MapProc;
import core.procedures.functional.Void;
import core.procedures.io.*;
import core.procedures.lists.AssocProc;
import core.procedures.lists.Length;
import core.procedures.lists.ListProc;
import core.procedures.lists.MemberProc;
import core.procedures.math.*;
import core.scm.SCMPredicate;
import core.procedures.strings.*;
import core.procedures.symbols.StringToSymbol;
import core.procedures.symbols.SymbolToString;
import core.procedures.system.*;
import core.procedures.vectors.*;
import core.scm.SCMEof;
import core.scm.SCMSymbol;
import core.scm.specialforms.*;

import java.util.ArrayList;
import java.util.List;

public final class DefaultEnvironment extends Environment {

  private static final AFn[] STANDARD_PROCEDURES = {
      /* System */
      new Exit(),
      new ClassOf(),
      new ErrorProc(),
      new Eval(),
      new Random(),
      new HashCode(),
      new ToString(),

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
      new Exp(),
      new Log(),
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
      new Numerator(),
      new Denominator(),
      new ToInexact(),
      new ToExact(),

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
      new StringToImmutableString(),
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
      new Display(),
      new Newline(),
      new Load(),
      new Read(),
      new Write(),
      new ReadChar(),
      new PeekChar(),
      new WriteChar(),
      new IsCharReady(),
      new CurrentInputPort(),
      new CurrentOutputPort(),
      new ClosePort(),
      new CloseInputPort(),
      new CloseOutputPort(),
      new OpenInputFile(),
      new OpenOutputFile(),
      new CallWithInputFile(),
      new CallWithOutputFile(),

      /* Pairs */
      new Length(),
      new MemberProc("member", new Equal()),
      new MemberProc("memq", new Eq()),
      new MemberProc("memv", new Eqv()),
      new AssocProc("assoc", new Equal()),
      new AssocProc("assq", new Eq()),
      new AssocProc("assv", new Eqv()),
      new ConsProc(),
      new Car(),
      new Cdr(),
      new SetCar(),
      new SetCdr(),
      new Append(),
      new Reverse(),
      new ListTail(),
      new ListRef(),
      new ListProc(),

      /* Symbols */
      new SymbolToString(),
      new StringToSymbol(),

      /* Vectors */
      new MakeVector(),
      new Vector(),
      new VectorImmutable(),
      new VectorLength(),
      new VectorRef(),
      new VectorSet(),
      new ListToVector(),
      new VectorToList(),
      new VectorFill(),
      new VectorToImmutableVector(),

      /* Functional */
      new Apply(),
      new MapProc(),
      new ForEach(),
      new Void(),

      /* Predicates */
      SCMPredicate.IS_NULL,
      SCMPredicate.IS_PAIR,
      SCMPredicate.IS_LIST,
      SCMPredicate.IS_PROMISE,
      SCMPredicate.IS_CHAR,
      SCMPredicate.IS_STRING,
      SCMPredicate.IS_VECTOR,
      SCMPredicate.IS_SYMBOL,
      SCMPredicate.IS_BOOLEAN,
      SCMPredicate.IS_PROC,
      SCMPredicate.IS_PORT,
      SCMPredicate.IS_INPUT_PORT,
      SCMPredicate.IS_OUTPUT_PORT,
      SCMPredicate.IS_NUMBER,
      SCMPredicate.IS_COMPLEX,
      SCMPredicate.IS_RATIONAL,
      SCMPredicate.IS_REAL,
      SCMPredicate.IS_EOF,
      SCMPredicate.IS_EXACT,
      SCMPredicate.IS_INEXACT,
      SCMPredicate.IS_ZERO,
      SCMPredicate.IS_EMPTY,
      SCMPredicate.IS_INTEGER,
      SCMPredicate.IS_POSITIVE,
      SCMPredicate.IS_NEGATIVE,
      SCMPredicate.IS_IMMUTABLE,
      SCMPredicate.IS_MUTABLE,
      };

  private static final ISpecialForm[] SPECIAL_FORMS = new ISpecialForm[] {
    Delay.DELAY,
    Quote.QUOTE,
    Set.SET,
    Quasiquote.QUASIQUOTE,
    Unquote.UNQUOTE,
    UnquoteSplicing.UNQUOTE_SPLICING,
    Time.TIME,
    /* With TCO */
    If.IF,
    Begin.BEGIN,
    And.AND,
    Or.OR,
    Lambda.LAMBDA,
    Define.DEFINE,
    Let.LET,
    LetRec.LETREC,
    LetSeq.LETSEQ,
    Do.DO,
    Case.CASE,
    Cond.COND,
    // TODO
    DefineSyntax.DEFINE_SYNTAX,
    LetSyntax.LET_SYNTAX,
    LetRecSyntax.LETREC_SYNTAX,
    SyntaxRules.SYNTAX_RULES
  };

  private static final List<String> LIBRARY_PROCEDURES = new ArrayList<>();
  static {
    // TODO Implement as Fns
    LIBRARY_PROCEDURES.add("(define (even? n) (= 0 (remainder n 2)))");
    LIBRARY_PROCEDURES.add("(define (odd? n) (not (even? n)))");

    //    LIBRARY_PROCEDURES.add("(define (quotient n m) (truncate (/ n m)))");

    // TODO Implement in Java?
    LIBRARY_PROCEDURES.add(
      "(define rationalize" +
          "  (letrec ((check (lambda (x) (if (not (real? x))" +
          "                                (error (string-append \"Wrong argument type. Expected: Real, actual: \"" +
          "                                                      (->string x))))))" +
          "           (find-between " +
          "            (lambda (lo hi)" +
          "              (if (integer? lo)" +
          "                  lo" +
          "                (let ((lo-int (floor lo))" +
          "                      (hi-int (floor hi)))" +
          "                  (if (< lo-int hi-int)" +
          "                      (+ 1 lo-int)" +
          "                    (+ lo-int" +
          "                       (/ (find-between (/ (- hi lo-int)) (/ (- lo lo-int))))))))))" +
          "           (do-find-between" +
          "            (lambda (lo hi)" +
          "              (cond" +
          "               ((negative? lo) (- (find-between (- hi) (- lo))))" +
          "               (else (find-between lo hi))))))" +
          "    (lambda (x within)" +
          "      (check x) (check within)" +
          "      (let* ((delta (abs within))" +
          "             (lo (- x delta))" +
          "             (hi (+ x delta)))" +
          "        (cond" +
          "         ((equal? x +nan.0) x)" +
          "         ((or (equal? x +inf.0) " +
          "              (equal? x -inf.0))" +
          "          (if (equal? delta +inf.0) +nan.0 x))" +
          "         ((equal? delta +inf.0) 0.0)" +
          "         ((not (= x x)) +nan.0)" +
          "         ((<= lo 0 hi) (if (exact? x) 0 0.0))" +
          "         ((or (inexact? lo) (inexact? hi))" +
          "          (exact->inexact (do-find-between (inexact->exact lo) (inexact->exact hi))))" +
          "         (else (do-find-between lo hi)))))))");
  }

  @Override
  public List<String> getLibraryProcedures() {
    return LIBRARY_PROCEDURES;
  }

  public DefaultEnvironment() {
    super(null);

    put(new SCMSymbol("eof"), SCMEof.EOF);

    /* Special Forms */
    for (ISpecialForm specialForm : SPECIAL_FORMS) {
      put(new SCMSymbol(specialForm.toString()), specialForm);
    }

    /* Standard Procedures */
    for (AFn proc : STANDARD_PROCEDURES) {
      put(new SCMSymbol(proc.getName()), proc);
    }
  }
}
