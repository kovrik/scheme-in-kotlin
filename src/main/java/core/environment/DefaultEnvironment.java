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
import core.procedures.lists.MemberProc;
import core.procedures.math.*;
import core.procedures.predicates.Predicate;
import core.procedures.strings.*;
import core.procedures.symbols.StringToSymbol;
import core.procedures.symbols.SymbolToString;
import core.procedures.system.ClassOf;
import core.procedures.system.ErrorProc;
import core.procedures.system.Eval;
import core.procedures.system.Exit;
import core.procedures.vectors.*;
import core.scm.SCMEof;
import core.scm.SCMSymbol;
import core.scm.specialforms.*;

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
import static core.scm.specialforms.Time.TIME;
import static core.scm.specialforms.Unquote.UNQUOTE;
import static core.scm.specialforms.UnquoteSplicing.UNQUOTE_SPLICING;

public final class DefaultEnvironment extends Environment {

  private static final AFn[] STANDARD_PROCEDURES = {
      /* System */
      new Exit(),
      new ClassOf(),
      new ErrorProc(),
      new Eval(),

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
      new Apply(),
      new MapProc(),
      new ForEach(),
      new Void(),

      /* Predicates */
      Predicate.IS_NULL,
      Predicate.IS_PAIR,
      Predicate.IS_LIST,
      Predicate.IS_PROMISE,
      Predicate.IS_CHAR,
      Predicate.IS_STRING,
      Predicate.IS_VECTOR,
      Predicate.IS_SYMBOL,
      Predicate.IS_BOOLEAN,
      Predicate.IS_PROC,
      Predicate.IS_PORT,
      Predicate.IS_INPUT_PORT,
      Predicate.IS_OUTPUT_PORT,
      Predicate.IS_NUMBER,
      Predicate.IS_RATIONAL,
      Predicate.IS_REAL,
      Predicate.IS_EOF,
      Predicate.IS_EXACT,
      Predicate.IS_INEXACT,
  };

  private static final Map<String, ISpecialForm> SPECIAL_FORMS = new HashMap<>();
  static {
    SPECIAL_FORMS.put(DELAY.toString(), DELAY);
    SPECIAL_FORMS.put(QUOTE.toString(), QUOTE);
    SPECIAL_FORMS.put(SET.toString(), SET);
    SPECIAL_FORMS.put(QUASIQUOTE.toString(), QUASIQUOTE);
    SPECIAL_FORMS.put(UNQUOTE.toString(), UNQUOTE);
    SPECIAL_FORMS.put(UNQUOTE_SPLICING.toString(), UNQUOTE_SPLICING);
    SPECIAL_FORMS.put(TIME.toString(), TIME);
    /* With TCO */
    SPECIAL_FORMS.put(If.IF.toString(), If.IF);
    SPECIAL_FORMS.put(Begin.BEGIN.toString(), Begin.BEGIN);
    SPECIAL_FORMS.put(And.AND.toString(), And.AND);
    SPECIAL_FORMS.put(Or.OR.toString(), Or.OR);
    SPECIAL_FORMS.put(Lambda.LAMBDA.toString(), Lambda.LAMBDA);
    SPECIAL_FORMS.put(Define.DEFINE.toString(), Define.DEFINE);
    SPECIAL_FORMS.put(Let.LET.toString(), Let.LET);
    SPECIAL_FORMS.put(LetRec.LETREC.toString(), LetRec.LETREC);
    SPECIAL_FORMS.put(LetSeq.LETSEQ.toString(), LetSeq.LETSEQ);
    SPECIAL_FORMS.put(Do.DO.toString(), Do.DO);
    SPECIAL_FORMS.put(Case.CASE.toString(), Case.CASE);
    SPECIAL_FORMS.put(Cond.COND.toString(), Cond.COND);

    // TODO
    SPECIAL_FORMS.put(DEFINE_SYNTAX.toString(), DEFINE_SYNTAX);
    SPECIAL_FORMS.put(LET_SYNTAX.toString(), LET_SYNTAX);
    SPECIAL_FORMS.put(LETREC_SYNTAX.toString(), LETREC_SYNTAX);
    SPECIAL_FORMS.put(SYNTAX_RULES.toString(), SYNTAX_RULES);
  }

  private static final List<String> LIBRARY_PROCEDURES = new ArrayList<>();
  static {
    // TODO Implement as Fns
    LIBRARY_PROCEDURES.add("(define (zero? n) (= n 0))");
    LIBRARY_PROCEDURES.add("(define (integer? x) (= x (round x)))");
    LIBRARY_PROCEDURES.add("(define (negative? n) (< n 0))");
    LIBRARY_PROCEDURES.add("(define (positive? n) (> n 0))");
    LIBRARY_PROCEDURES.add("(define (even? n) (= 0 (remainder n 2)))");
    LIBRARY_PROCEDURES.add("(define (odd? n) (not (even? n)))");

    // FIXME Should work
//    LIBRARY_PROCEDURES.add("(define (quotient n m) (truncate (/ n m)))");

    LIBRARY_PROCEDURES.add("(define empty? null?)");
    LIBRARY_PROCEDURES.add("(define (list . elements) elements)");

    // simple map
    // FIXME Implement variadic map proc
//    LIBRARY_PROCEDURES.add("(define (map proc lis)" +
//                           "   (cond ((null? lis) '())" +
//                           "         ((pair? lis) (cons (proc (car lis))" +
//                           "                            (map proc (cdr lis))))" +
//                           "          (else (error \"Not a proper list!\"))))");
  }

  @Override
  public List<String> getLibraryProcedures() {
    return LIBRARY_PROCEDURES;
  }

  public DefaultEnvironment() {
    super(null);

    put(new SCMSymbol("eof"), SCMEof.EOF);

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
