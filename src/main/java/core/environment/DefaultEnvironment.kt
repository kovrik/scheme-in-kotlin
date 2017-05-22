package core.environment

import core.procedures.bit.*
import core.procedures.bit.BitSet
import core.procedures.characters.CharComparison
import core.procedures.characters.CharPredicate
import core.procedures.characters.CharProc
import core.procedures.characters.IntegerToChar
import core.procedures.cons.*
import core.procedures.delayed.*
import core.procedures.equivalence.Eq
import core.procedures.equivalence.Equal
import core.procedures.equivalence.Eqv
import core.procedures.equivalence.Identical
import core.procedures.exceptions.ExData
import core.procedures.exceptions.ExInfo
import core.procedures.functional.Apply
import core.procedures.functional.ForEach
import core.procedures.functional.MapProc
import core.procedures.functional.VoidProc
import core.procedures.generic.*
import core.procedures.hashmaps.*
import core.procedures.interop.*
import core.procedures.io.*
import core.procedures.keywords.KeywordProc
import core.procedures.lists.ListProc
import core.procedures.lists.MemberProc
import core.procedures.math.*
import core.procedures.math.complex.*
import core.procedures.math.trigonometry.*
import core.procedures.meta.MetaProc
import core.procedures.meta.WIthMeta
import core.procedures.predicates.Predicate
import core.procedures.sets.*
import core.procedures.strings.*
import core.procedures.symbols.StringToSymbol
import core.procedures.symbols.SymbolToString
import core.procedures.system.*
import core.procedures.vectors.*
import core.scm.Symbol
import core.scm.specialforms.*
import core.scm.specialforms.Set
import java.util.*

class DefaultEnvironment : Environment(null) {

    val libraryProcedures: List<String>
        get() = LIBRARY_PROCEDURES

    init {

        /* Special Forms */
        for (specialForm in SPECIAL_FORMS) {
            put(Symbol.intern(specialForm.toString()), specialForm)
        }

        /* Standard Procedures */
        for (proc in STANDARD_PROCEDURES) {
            put(Symbol.intern(proc.name), proc)
        }

        /* Constants and special cases, synonyms*/
        put(Symbol.intern("pi"), Math.PI)
        put(Symbol.intern("nil"), null)
        put(Symbol.intern("null"), null)
        put(Symbol.intern("eof"), null)
        put(Symbol.intern("call/cc"), get(Symbol.intern("call-with-current-continuation")))
        put(Symbol.intern("def"), get(Symbol.intern("define")))
        put(Symbol.intern("fn"), get(Symbol.intern("lambda")))
    }

    companion object {

        private val STANDARD_PROCEDURES = arrayOf(
                /* Primitive Types */
                PrimitiveNumberType.BYTE,
                PrimitiveNumberType.SHORT,
                PrimitiveNumberType.INT,
                PrimitiveNumberType.LONG,
                PrimitiveNumberType.DOUBLE,
                PrimitiveNumberType.FLOAT,
                BooleanType(),
                CharType(),
                BigIntegerType(),
                BigDecimalType(),

                /* System */
                Exit(),
                IsInstance(),
                Cast(),
                ClassProc(),
                object : ClassProc() { override val name = "class-of" },
                ErrorProc(),
                Pst(),
                Eval(),
                RandomProc(),
                HashCode(),
                object : HashCode() { override val name = "hash" },
                ToString(), object : ToString() { override val name = "str" },
                Name(),
                Identity(),
                Num(),
                Sleep(),

                /* Delayed */
                Force(),
                PromiseProc(),
                Deliver(),
                Deref(),
                FutureCancel(),

                /* Math */
                Negation(),
                Addition(),
                Subtraction(),
                Multiplication(),
                Division(),
                Abs(),
                Sqrt(),
                Expt(),
                Exp(),
                Log(),
                Modulo(),
                object : Modulo() { override val name = "mod" },
                Remainder(),
                Quotient(),
                object : Quotient() { override val name = "quot" },
                Round(),
                Floor(),
                Ceiling(),
                Truncate(),
                Max(),
                Min(),
                GCD(),
                LCM(),
                Numerator(),
                Denominator(),
                ToInexact(),
                ToExact(),
                RealPart(),
                ImagPart(),
                Magnitude(),
                Angle(),
                MakePolar(),
                MakeRectangular(),

                /* Trigonometry */
                Sin(),
                Sinh(),
                Cos(),
                Cosh(),
                Tan(),
                Tanh(),
                Asin(),
                Acos(),
                Atan(),

                /* Comparison & Equality */
                NumericalComparison.EQUAL,
                NumericalComparison.LESS,
                NumericalComparison.LESS_EQUAL,
                NumericalComparison.GREATER,
                NumericalComparison.GREATER_EQUAL,
                Identical(),
                Eq(),
                Eqv(),
                Equal(),

                /* Strings */
                StringLength(),
                StringCopy(),
                StringProc(),
                Substring(),
                object : Substring() { override val name = "subs" },
                StringAppend(),
                StringFill(),
                MakeString(),
                ListToString(),
                NumberToString(),
                StringToNumber(),
                StringToList(),
                StringRef(),
                StringSet(),
                StringToImmutableString(),
                StringToMutableString(),
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
                EndsWith(),
                Includes(),
                IndexOf(),
                LastIndexOf(),
                Lowercase(),
                StartsWith(),
                Trim(),
                Uppercase(),
                Split(),
                Join(),
                Replace(),
                ReplaceFirst(),
                ReFind(),
                ReMatcher(),
                RePattern(),
                ReGroups(),

                /* Characters */
                IntegerToChar(),
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
                Display(),
                object : Display() { override val name = "print" },
                Println(),
                Newline(),
                Load(),
                Read(),
                Write(),
                ReadChar(),
                PeekChar(),
                WriteChar(),
                IsCharReady(),
                CurrentInputPort(),
                CurrentOutputPort(),
                ClosePort(),
                CloseInputPort(),
                CloseOutputPort(),
                OpenInputFile(),
                OpenOutputFile(),
                CallWithInputFile(),
                CallWithOutputFile(),

                /* Pairs */
                MemberProc("member", Equal()),
                MemberProc("memq", Eq()),
                MemberProc("memv", Eqv()),
                AssocProc("assoc", Equal()),
                AssocProc("assq", Eq()),
                AssocProc("assv", Eqv()),
                ConsProc(),
                Car(),
                Cdr(),
                SetCar(),
                SetCdr(),
                Append(),
                Reverse(),
                ListTail(),
                ListRef(),
                ListProc(),

                /* Symbols */
                SymbolToString(),
                StringToSymbol(),
                object : StringToSymbol() { override val name = "symbol" },

                /* Vectors */
                MakeVector(),
                VectorProc(),
                VectorImmutable(),
                VectorLength(),
                VectorRef(),
                VectorSet(),
                ListToVector(),
                VectorToList(),
                VectorFill(),
                VectorToImmutableVector(),
                Vec(),
                Shuffle(),

                /* Functional */
                Apply(),
                MapProc(),
                ForEach(),
                VoidProc(),

                /* Hashmaps */
                Find(),
                HashMapProc(),
                Put(),
                Key(),
                Keys(),
                Val(),
                Vals(),
                MapInvert(),
                Merge(),
                Zipmap(),

                /* Sets */
                SetProc(),
                Union(),
                Intersection(),
                Difference(),
                IsSubset(),
                IsSuperset(),

                /* Generic */
                Count(),
                Get(),
                Nth(),
                Sort(),
                object : Count() { override val name = "length" },
                Conj(),
                Empty(),
                Range(),
                First(),
                Second(),
                Next(),
                object : Next() { override val name = "rest" },
                RandNth(),

                /* Keywords */
                KeywordProc(),

                /* Meta */
                MetaProc(),
                WIthMeta(),

                /* Exceptions */
                ExData(),
                ExInfo(),

                /* Bitwise */
                BitAnd(),
                BitAndNot(),
                BitClear(),
                BitFlip(),
                BitNot(),
                BitOr(),
                BitSet(),
                BItShiftLeft(),
                BitShiftRight(),
                BitTest(),
                BitXor(),

                /* Predicates */
                Predicate.IS_NULL,
                Predicate.IS_NIL,
                Predicate.IS_EOF,
                Predicate.IS_SOME,
                Predicate.IS_PAIR,
                Predicate.IS_LIST,
                Predicate.IS_SET,
                Predicate.IS_MAP,
                Predicate.IS_MAP_ENTRY,
                Predicate.IS_COLL,
                Predicate.IS_PROMISE,
                Predicate.IS_FUTURE,
                Predicate.IS_FUTURE_DONE,
                Predicate.IS_FUTURE_CANCELLED,
                Predicate.IS_DELAY,
                Predicate.IS_REALIZED,
                Predicate.IS_CHAR,
                Predicate.IS_STRING,
                Predicate.IS_VECTOR,
                Predicate.IS_SYMBOL,
                Predicate.IS_BOOLEAN,
                Predicate.IS_TRUE,
                Predicate.IS_FALSE,
                Predicate.IS_PROC,
                Predicate.IS_PORT,
                Predicate.IS_INPUT_PORT,
                Predicate.IS_OUTPUT_PORT,
                Predicate.IS_NUMBER,
                Predicate.IS_COMPLEX,
                Predicate.IS_RATIONAL,
                Predicate.IS_RATIO,
                Predicate.IS_REAL,
                Predicate.IS_EXACT,
                Predicate.IS_INEXACT,
                Predicate.IS_ZERO,
                Predicate.IS_EVEN,
                Predicate.IS_ODD,
                Predicate.IS_EMPTY,
                Predicate.IS_INTEGER,
                Predicate.IS_POSITIVE,
                Predicate.IS_POS,
                Predicate.IS_NEGATIVE,
                Predicate.IS_NEG,
                Predicate.IS_IMMUTABLE,
                Predicate.IS_MUTABLE,
                Predicate.IS_KEYWORD,
                Predicate.IS_ANY,
                Predicate.IS_BLANK,
                Predicate.IS_CLASS,
                Predicate.IS_DECIMAL,
                Predicate.IS_FLOAT,
                Predicate.IS_FN)

        private val SPECIAL_FORMS = arrayOf<ISpecialForm>(
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
                When.WHEN,
                Unless.UNLESS,
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
                Dot.DOT,
                Throw.THROW,
                Try.TRY,
                DynamicWind.DYNAMIC_WIND,
                CallCC.CALL_WITH_CURRENT_CONTINUATION,
                CallCC.CALL_CC,
                // TODO Macros
                DefineSyntax.DEFINE_SYNTAX,
                LetSyntax.LET_SYNTAX,
                LetRecSyntax.LETREC_SYNTAX,
                SyntaxRules.SYNTAX_RULES)

        private val LIBRARY_PROCEDURES = ArrayList<String>()

        init {
            /* Naive implementations (not via Continuations) */
            // TODO attach Metadata and mark these as pure?
            LIBRARY_PROCEDURES.add("(define values list)")
            LIBRARY_PROCEDURES.add("(define (call-with-values producer consumer) (apply consumer (producer)))")

            LIBRARY_PROCEDURES.add("(define (add1 n) (+ n 1))")
            LIBRARY_PROCEDURES.add("(define (inc  n) (+ n 1))")
            LIBRARY_PROCEDURES.add("(define (dec  n) (- n 1))")
            LIBRARY_PROCEDURES.add(
                    "(define rationalize" +
                            "  (letrec ((check (lambda (x) (when (not (real? x))" +
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
                            "         (else (do-find-between lo hi)))))))")
        }
    }
}