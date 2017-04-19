package core.environment;

import core.procedures.AFn;
import core.procedures.characters.CharComparison;
import core.procedures.characters.CharPredicate;
import core.procedures.characters.CharProc;
import core.procedures.characters.IntegerToChar;
import core.procedures.cons.*;
import core.procedures.continuations.CallCC;
import core.procedures.continuations.DynamicWind;
import core.procedures.delayed.Deliver;
import core.procedures.delayed.Deref;
import core.procedures.delayed.Force;
import core.procedures.delayed.FutureCancel;
import core.procedures.delayed.Promise;
import core.procedures.equivalence.Eq;
import core.procedures.equivalence.Equal;
import core.procedures.equivalence.Eqv;
import core.procedures.equivalence.Identical;
import core.procedures.functional.Apply;
import core.procedures.functional.ForEach;
import core.procedures.functional.MapProc;
import core.procedures.functional.Void;
import core.procedures.generic.*;
import core.procedures.hashmaps.*;
import core.procedures.interop.BooleanType;
import core.procedures.interop.CharType;
import core.procedures.interop.PrimitiveNumberType;
import core.procedures.io.*;
import core.procedures.keywords.Keyword;
import core.procedures.lists.ListProc;
import core.procedures.lists.MemberProc;
import core.procedures.math.*;
import core.procedures.math.complex.*;
import core.procedures.math.trigonometry.*;
import core.procedures.predicates.SCMPredicate;
import core.procedures.sets.*;
import core.procedures.strings.*;
import core.procedures.symbols.StringToSymbol;
import core.procedures.symbols.SymbolToString;
import core.procedures.system.*;
import core.procedures.vectors.*;
import core.scm.SCMNil;
import core.scm.SCMSymbol;
import core.scm.specialforms.*;

import java.util.ArrayList;
import java.util.List;

public final class DefaultEnvironment extends Environment {

  private static final AFn[] STANDARD_PROCEDURES = {
      /* Primitive Types */
      PrimitiveNumberType.BYTE,
      PrimitiveNumberType.SHORT,
      PrimitiveNumberType.INT,
      PrimitiveNumberType.LONG,
      PrimitiveNumberType.DOUBLE,
      PrimitiveNumberType.FLOAT,
      new BooleanType(),
      new CharType(),

      /* System */
      new Exit(),
      new ClassOf(),
      new ClassOf() { @Override public String getName() { return "type"; } },
      new ClassOf() { @Override public String getName() { return "class"; } },
      new ErrorProc(),
      new Eval(),
      new RandomProc(),
      new HashCode(),
      new ToString(),
      new Name(),
      new Identity(),

      /* Delayed */
      new Force(),
      new Promise(),
      new Deliver(),
      new Deref(),
      new FutureCancel(),

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
      new RealPart(),
      new ImagPart(),
      new Magnitude(),
      new Angle(),
      new MakePolar(),
      new MakeRectangular(),

      /* Trigonometry */
      new Sin(),
      new Sinh(),
      new Cos(),
      new Cosh(),
      new Tan(),
      new Tanh(),
      new Asin(),
      new Acos(),
      new Atan(),

      /* Comparison & Equality */
      NumericalComparison.EQUAL,
      NumericalComparison.LESS,
      NumericalComparison.LESS_EQUAL,
      NumericalComparison.GREATER,
      NumericalComparison.GREATER_EQUAL,
      new Identical(),
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
      new StringToMutableString(),
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
      new EndsWith(),
      new Includes(),
      new IndexOf(),
      new LastIndexOf(),
      new Lowercase(),
      new StartsWith(),
      new Trim(),
      new Uppercase(),

      /* Characters */
      new IntegerToChar(),
      CharPredicate.IS_CHAR_WHITESPACE,
      CharPredicate.IS_CHAR_ALPHABETIC,
      CharPredicate.IS_CHAR_UPPER_CASE,
      CharPredicate.IS_CHAR_LOWER_CASE,
      CharPredicate.IS_CHAR_NUMERIC,
      CharProc.CHAR_TO_INTEGER,
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
      new Display() { @Override public String getName() { return "print"; } },
      new Println(),
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
      new StringToSymbol() { @Override public String getName() { return "symbol"; } },

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
      new Vec(),

      /* Functional */
      new Apply(),
      new MapProc(),
      new ForEach(),
      new Void(),

      /* Continuations */
      new CallCC(),
      new DynamicWind(),

      /* Hashmaps */
      new Find(),
      new HashMapProc(),
      new Put(),
      new Key(),
      new Keys(),
      new Val(),
      new Vals(),
      new MapInvert(),
      new Merge(),
      new Zipmap(),

      /* Sets */
      new SetProc(),
      new Union(),
      new Intersection(),
      new Difference(),
      new IsSubset(),
      new IsSuperset(),

      /* Generic */
      new Count(),
      new Get(),
      new Nth(),
      new Sort(),
      new Count() { @Override public String getName() { return "length"; } },
      new Conj(),
      new Empty(),

      /* Keywords */
      new Keyword(),

      /* Predicates */
      SCMPredicate.IS_NULL,
      SCMPredicate.IS_SOME,
      SCMPredicate.IS_PAIR,
      SCMPredicate.IS_LIST,
      SCMPredicate.IS_SET,
      SCMPredicate.IS_MAP,
      SCMPredicate.IS_MAP_ENTRY,
      SCMPredicate.IS_COLL,
      SCMPredicate.IS_PROMISE,
      SCMPredicate.IS_FUTURE,
      SCMPredicate.IS_FUTURE_DONE,
      SCMPredicate.IS_FUTURE_CANCELLED,
      SCMPredicate.IS_DELAY,
      SCMPredicate.IS_REALIZED,
      SCMPredicate.IS_CHAR,
      SCMPredicate.IS_STRING,
      SCMPredicate.IS_VECTOR,
      SCMPredicate.IS_SYMBOL,
      SCMPredicate.IS_BOOLEAN,
      SCMPredicate.IS_TRUE,
      SCMPredicate.IS_FALSE,
      SCMPredicate.IS_PROC,
      SCMPredicate.IS_PORT,
      SCMPredicate.IS_INPUT_PORT,
      SCMPredicate.IS_OUTPUT_PORT,
      SCMPredicate.IS_NUMBER,
      SCMPredicate.IS_COMPLEX,
      SCMPredicate.IS_RATIONAL,
      SCMPredicate.IS_RATIO,
      SCMPredicate.IS_REAL,
      SCMPredicate.IS_EXACT,
      SCMPredicate.IS_INEXACT,
      SCMPredicate.IS_ZERO,
      SCMPredicate.IS_EVEN,
      SCMPredicate.IS_ODD,
      SCMPredicate.IS_EMPTY,
      SCMPredicate.IS_INTEGER,
      SCMPredicate.IS_POSITIVE,
      SCMPredicate.IS_NEGATIVE,
      SCMPredicate.IS_IMMUTABLE,
      SCMPredicate.IS_MUTABLE,
      SCMPredicate.IS_KEYWORD,
      SCMPredicate.IS_ANY,
      };

  private static final ISpecialForm[] SPECIAL_FORMS = new ISpecialForm[] {
    Delay.DELAY,
    Future.FUTURE,
    Quote.QUOTE,
    Set.SET,
    Quasiquote.QUASIQUOTE,
    Unquote.UNQUOTE,
    UnquoteSplicing.UNQUOTE_SPLICING,
    Time.TIME,
    Assert.ASSERT,
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
    Else.ELSE,
    New.NEW,
    Comment.COMMENT,
    // TODO Macros
    DefineSyntax.DEFINE_SYNTAX,
    LetSyntax.LET_SYNTAX,
    LetRecSyntax.LETREC_SYNTAX,
    SyntaxRules.SYNTAX_RULES
  };

  private static final List<String> LIBRARY_PROCEDURES = new ArrayList<>();
  static {
    /* Naive implementations (not via Continuations) */
    // TODO attach Metadata and mark these as pure?
    LIBRARY_PROCEDURES.add("(define values list)");
    LIBRARY_PROCEDURES.add("(define (call-with-values producer consumer) (apply consumer (producer)))");

    LIBRARY_PROCEDURES.add("(define (add1 n) (+ n 1))");
    LIBRARY_PROCEDURES.add("(define (inc  n) (+ n 1))");
    LIBRARY_PROCEDURES.add("(define (dec  n) (- n 1))");

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

    /* Special Forms */
    for (ISpecialForm specialForm : SPECIAL_FORMS) {
      put(SCMSymbol.of(specialForm.toString()), specialForm);
    }

    /* Standard Procedures */
    for (AFn proc : STANDARD_PROCEDURES) {
      put(SCMSymbol.of(proc.getName()), proc);
    }

    /* Constants and special cases, synonyms*/
    put(SCMSymbol.of("pi"),          Math.PI);
    put(SCMSymbol.of("nil"),         SCMNil.NIL);
    put(SCMSymbol.of("null"),        SCMNil.NIL);
    put(SCMSymbol.of("eof"),         SCMNil.NIL);
    put(SCMSymbol.of("call/cc"),     get(SCMSymbol.of("call-with-current-continuation")));
    put(SCMSymbol.of("nil?"),        get(SCMSymbol.of("null?")));
    put(SCMSymbol.of("eof-object?"), get(SCMSymbol.of("null?")));
    put(SCMSymbol.of("str"),         get(SCMSymbol.of("->string")));
    put(SCMSymbol.of("hash"),        get(SCMSymbol.of("hashcode")));
    put(SCMSymbol.of("first"),       get(SCMSymbol.of("car")));
    put(SCMSymbol.of("next"),        get(SCMSymbol.of("cdr")));
    put(SCMSymbol.of("def"),         get(SCMSymbol.of("define")));
    put(SCMSymbol.of("fn"),          get(SCMSymbol.of("lambda")));
  }
}
