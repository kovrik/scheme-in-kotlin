package core.environment

import core.procedures.AFn
import core.procedures.arrays.*
import core.procedures.bit.*
import core.procedures.booleans.Not
import core.procedures.booleans.Xor
import core.procedures.box.*
import core.procedures.bytes.BytesToString
import core.procedures.bytes.StringToBytes
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
import core.procedures.collections.*
import core.procedures.hashmaps.*
import core.procedures.interop.*
import core.procedures.io.*
import core.procedures.keywords.KeywordProc
import core.procedures.lists.*
import core.procedures.math.*
import core.procedures.math.complex.*
import core.procedures.math.trigonometry.*
import core.procedures.meta.ArityProc
import core.procedures.meta.MetaProc
import core.procedures.meta.WIthMeta
import core.procedures.predicates.Predicate
import core.procedures.seqs.*
import core.procedures.sets.*
import core.procedures.strings.*
import core.procedures.symbols.Gensym
import core.procedures.symbols.StringToSymbol
import core.procedures.symbols.SymbolToString
import core.procedures.system.*
import core.procedures.time.CurrentMilliseconds
import core.procedures.time.CurrentSeconds
import core.procedures.time.NanoTime
import core.procedures.vectors.*
import core.scm.Symbol
import core.scm.specialforms.*
import core.scm.specialforms.SetForm
import core.scm.specialforms.Swap
import kotlin.math.E
import kotlin.math.PI

class DefaultEnvironment : Environment(null) {

    val libraryProcedures: List<String>
        get() = LIBRARY_PROCEDURES

    init {
        /* Special Forms */
        SPECIAL_FORMS.forEach { put(Symbol.intern(it.toString()), it) }
        /* Standard Procedures */
        STANDARD_PROCEDURES.forEach { put(Symbol.intern(it.name), it) }
        /* Constants and special cases, synonyms*/
        put(Symbol.intern("pi"),        PI)
        put(Symbol.intern("euler.0"),   E)
        put(Symbol.intern("phi.0"),     1.618033988749895)
        put(Symbol.intern("gamma.0"),   0.5772156649015329)
        put(Symbol.intern("catalan.0"), 0.915965594177219)
        put(Symbol.intern("eof"),       null)
        put(Symbol.intern("call/cc"),   get(CallCC.symbol))
        put(Symbol.intern("def"),       get(Define.symbol))
        put(Symbol.intern("fn"),        get(Lambda.symbol))
        put(Symbol.intern("\$e"),       Unit)
    }

    companion object {

        private val STANDARD_PROCEDURES = arrayOf<AFn<*, *>>(
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

                /* Arrays */
                ArraysNew.Bytes(),
                ArraysNew.Shorts(),
                ArraysNew.Ints(),
                ArraysNew.Longs(),
                ArraysNew.Doubles(),
                ArraysNew.Floats(),
                ArraysNew.Chars(),
                ArraysNew.Booleans(),
                ArraysNew.Objects(),
                ArraysNew.ArrayNew(),
                object : ArraysNew.Bytes()    { override val name = "byte-array"    },
                object : ArraysNew.Shorts()   { override val name = "short-array"   },
                object : ArraysNew.Ints()     { override val name = "int-array"     },
                object : ArraysNew.Longs()    { override val name = "long-array"    },
                object : ArraysNew.Doubles()  { override val name = "double-array"  },
                object : ArraysNew.Floats()   { override val name = "float-array"   },
                object : ArraysNew.Chars()    { override val name = "char-array"    },
                object : ArraysNew.Booleans() { override val name = "boolean-array" },
                object : ArraysNew.Objects()  { override val name = "object-array"  },

                ArraysAppend.BytesAppend(),
                ArraysAppend.ShortsAppend(),
                ArraysAppend.IntsAppend(),
                ArraysAppend.LongsAppend(),
                ArraysAppend.DoublesAppend(),
                ArraysAppend.FloatsAppend(),
                ArraysAppend.CharsAppend(),
                ArraysAppend.BooleansAppend(),
                ArraysAppend.ObjectsAppend(),

                ArraysFill.BytesFill(),
                ArraysFill.ShortsFill(),
                ArraysFill.IntsFill(),
                ArraysFill.LongsFill(),
                ArraysFill.DoublesFill(),
                ArraysFill.FloatsFill(),
                ArraysFill.CharsFill(),
                ArraysFill.BooleansFill(),
                ArraysFill.ObjectsFill(),

                ArraysLength.BytesLength(),
                ArraysLength.ShortsLength(),
                ArraysLength.IntsLength(),
                ArraysLength.LongsLength(),
                ArraysLength.DoublesLength(),
                ArraysLength.FloatsLength(),
                ArraysLength.CharsLength(),
                ArraysLength.BooleansLength(),
                ArraysLength.ObjectsLength(),

                ArraysRef.BytesRef(),
                ArraysRef.ShortsRef(),
                ArraysRef.IntsRef(),
                ArraysRef.LongsRef(),
                ArraysRef.DoublesRef(),
                ArraysRef.FloatsRef(),
                ArraysRef.CharsRef(),
                ArraysRef.BooleansRef(),
                ArraysRef.ObjectsRef(),

                ArraysSet.BytesSet(),
                ArraysSet.ShortsSet(),
                ArraysSet.IntsSet(),
                ArraysSet.LongsSet(),
                ArraysSet.DoublesSet(),
                ArraysSet.FloatsSet(),
                ArraysSet.CharsSet(),
                ArraysSet.BooleansSet(),
                ArraysSet.ObjectsSet(),

                ArraysToList.BytesToList(),
                ArraysToList.ShortsToList(),
                ArraysToList.IntsToList(),
                ArraysToList.LongsToList(),
                ArraysToList.DoublesToList(),
                ArraysToList.FloatsToList(),
                ArraysToList.CharsToList(),
                ArraysToList.BooleansToList(),
                ArraysToList.ObjectsToList(),

                ListToArrays.ListToBytes(),
                ListToArrays.ListToShorts(),
                ListToArrays.ListToInts(),
                ListToArrays.ListToLongs(),
                ListToArrays.ListToDoubles(),
                ListToArrays.ListToFloats(),
                ListToArrays.ListToChars(),
                ListToArrays.ListToBooleans(),
                ListToArrays.ListToObjects(),

                MakeArrays.MakeBytes(),
                MakeArrays.MakeShorts(),
                MakeArrays.MakeInts(),
                MakeArrays.MakeLongs(),
                MakeArrays.MakeDoubles(),
                MakeArrays.MakeFloats(),
                MakeArrays.MakeChars(),
                MakeArrays.MakeBooleans(),
                MakeArrays.MakeObjects(),
                MakeArrays.MakeArray(),

                SubArrays.SubBytes(),
                SubArrays.SubShorts(),
                SubArrays.SubInts(),
                SubArrays.SubLongs(),
                SubArrays.SubDoubles(),
                SubArrays.SubFloats(),
                SubArrays.SubChars(),
                SubArrays.SubBooleans(),
                SubArrays.SubObjects(),

                ToArray(),

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
                RandProc(),
                CryptoRandomBytes(),
                HashCode(),
                object : HashCode() { override val name = "hash" },
                ToString(), object : ToString() { override val name = "str" },
                Name(),
                Identity(),
                Num(),
                Sleep(),

                /* Time */
                CurrentSeconds(),
                CurrentMilliseconds(),
                NanoTime(),

                /* Delayed */
                Force(),
                PromiseProc(),
                object : PromiseProc() { override val name = "make-promise" },
                Deliver(),
                Deref(),
                FutureCancel(),
                CurrentThread(),
                ThreadWait(),
                ThreadInterrupt(),

                /* Booleans */
                Not(),
                Xor(),

                /* Math */
                Addition(),
                Subtraction(),
                Multiplication(),
                Division(),
                Abs(),
                Sqrt(),
                Sgn(),
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
                Conjugate(),

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
                DegreesToRadians(),
                RadiansToDegrees(),

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
                CharPredicate.IS_CHAR_TITLE_CASE,
                CharPredicate.IS_CHAR_ISO_CONTROL,
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
                ReadSyntax(),
                Write(),
                ReadChar(),
                ReadByte(),
                ReadBytes(),
                ReadLine(),
                ReadString(),
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
                ConsProc(),
                PairProc(),
                Mcons(),
                Car(),
                Cdr(),
                Mcar(),
                Mcdr(),
                SetMcar(),
                SetMcdr(),
                Append(),
                Reverse(),
                ListTail(),
                ListRef(),
                ListProc(),

                /* Symbols */
                SymbolToString(),
                StringToSymbol(),
                object : StringToSymbol() { override val name = "symbol" },
                Gensym(),

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
                HashMapImmutableProc(),
                HashMapToImmutableHashMap(),
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
                AssocProc("assoc", Equal()),
                AssocProc("assq", Eq()),
                AssocProc("assv", Eqv()),
                Empty(),

                /* Keywords */
                KeywordProc(),

                /* Meta */
                MetaProc(),
                WIthMeta(),

                /* Arity */
                ArityProc(),

                /* Exceptions */
                ExData(),
                ExInfo(),

                /* Bitwise */
                BitAnd(),
                object : BitAnd() { override val name = "bitwise-and" },
                BitAndNot(),
                BitClear(),
                BitFlip(),
                BitNot(),
                object : BitNot() { override val name = "bitwise-not" },
                BitOr(),
                object : BitOr() { override val name = "bitwise-ior" },
                BitSet(),
                BItShiftLeft(),
                BitShiftRight(),
                ArithmeticShift(),
                BitTest(),
                object: BitTest() { override val name = "bitwise-bit-set?" },
                BitXor(),
                object : BitXor() { override val name = "bitwise-xor" },

                /* Bytes */
                BytesToString(),
                StringToBytes(),

                /* Boxes (Atoms) */
                BoxProc(),
                object : BoxProc() { override val name = "atom" },
                Unbox(),
                SetBox(),
                BoxCas(),
                object : BoxCas() { override val name = "compare-and-set!" },
                Reset(),
                MakeWeakBox(),
                WeakBoxValue(),

                /* Seqs */
                Butlast(),
                Concat(),
                Conj(),
                ConsSeqProc(),
                Count(),
                CycleProc(),
                object : Count() { override val name = "length" },
                Drop(),
                DropLast(),
                First(),
                Flatten(),
                Get(),
                Into(),
                Iterate(),
                Last(),
                Nth(),
                Next(),
                Range(),
                RandNth(),
                RepeatProc(),
                Repeatedly(),
                Rest(),
                Second(),
                Seq(),
                Sort(),
//                Some(),
                Take(),
                TakeLast(),

                /* Predicates */
                Predicate.IS_NULL,
                Predicate.IS_NIL,
                Predicate.IS_EOF,
                Predicate.IS_SOME,
                Predicate.IS_PROC,
                Predicate.IS_FN,
                Predicate.IS_IFN,
                Predicate.IS_PAIR,
                Predicate.IS_MPAIR,
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
                Predicate.IS_DELAY_FORCED,
                Predicate.IS_CHAR,
                Predicate.IS_STRING,
                Predicate.IS_VECTOR,
                Predicate.IS_SYMBOL,
                Predicate.IS_BOOLEAN,
                Predicate.IS_TRUE,
                Predicate.IS_FALSE,
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
                Predicate.IS_NAN,
                Predicate.IS_FINITE,
                Predicate.IS_INFINITE,
                Predicate.IS_EXACT_NONNEGATIVE_INTEGER,
                Predicate.IS_NATURAL,
                Predicate.IS_POSITIVE_INTEGER,
                Predicate.IS_NEGATIVE_INTEGER,
                Predicate.IS_NONPOSITIVE_INTEGER,
                Predicate.IS_NONNEGATIVE_INTEGER,
                Predicate.IS_EMPTY,
                Predicate.IS_INTEGER,
                Predicate.IS_EXACT_INTEGER,
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
                Predicate.IS_BOX,
                Predicate.IS_WEAK_BOX,
                Predicate.IS_ATOM,
                Predicate.IS_BYTE,
                Predicate.IS_ARRAY,
                Predicate.IS_BOOLEANS,
                Predicate.IS_BYTES,
                Predicate.IS_CHARS,
                Predicate.IS_DOUBLES,
                Predicate.IS_FLOATS,
                Predicate.IS_INTS,
                Predicate.IS_LONGS,
                Predicate.IS_SHORTS,
                Predicate.IS_OBJECTS,
                Predicate.IS_VOID,
                Predicate.IS_THREAD,
                Predicate.IS_THREAD_RUNNING,
                Predicate.IS_THREAD_DEAD,
                Predicate.IS_SYNTAX,
                Predicate.IS_SEQUENCE,
                Predicate.IS_SEQ,
                Predicate.IS_SEQABLE,
                Predicate.IS_ARITY)

        private val SPECIAL_FORMS = arrayOf(
                ThreadForm,
                DelayForm,
                LazyForm,
                LazySeqForm,
                FutureForm,
                Quote,
                SetForm,
                Quasiquote,
                Unquote,
                UnquoteSplicing,
                Time,
                Assert,
                /* With TCO */
                If,
                When,
                Unless,
                Begin,
                And,
                Nand,
                Or,
                Nor,
                Lambda,
                Define,
                Let,
                LetRec,
                LetSeq,
                Do,
                Dotimes,
                Case,
                Cond,
                Else,
                New,
                Comment,
                Dot,
                Throw,
                Try,
                DynamicWind,
                CallCC,
                ThunkForm,
                SyntaxForm,
                Locking,
                Swap,
                // TODO Macros
                DefineSyntax,
                LetSyntax,
                LetRecSyntax,
                SyntaxRules)

        private val LIBRARY_PROCEDURES = listOf(
            /* Naive implementations (not via Continuations) */
            // TODO attach Metadata and mark these as pure?
            "(define values list)",
            "(define (call-with-values producer consumer) (apply consumer (producer)))",

            "(define (caar p) (car (car p)))",
            "(define (cadr p) (car (cdr p)))",
            "(define (cdar p) (cdr (car p)))",
            "(define (cddr p) (cdr (cdr p)))",
            "(define (add1 n) (+ n 1))",
            "(define (sub1 n) (- n 1))",
            "(define (inc  n) (+ n 1))",
            "(define (dec  n) (- n 1))",
            "(define (sqr  n) (* n n))",
            """(define rationalize
                     (letrec ((check (lambda (x)
                                       (when (not (real? x))
                                         (error (string-append "Wrong argument type. Expected: Real, actual: "
                                                                (->string x))))))
                              (find-between
                               (lambda (lo hi)
                                 (if (integer? lo)
                                     lo
                                   (let ((lo-int (floor lo))
                                         (hi-int (floor hi)))
                                     (if (< lo-int hi-int)
                                         (+ 1 lo-int)
                                       (+ lo-int
                                          (/ (find-between (/ (- hi lo-int)) (/ (- lo lo-int))))))))))
                              (do-find-between
                               (lambda (lo hi)
                                 (cond
                                  ((negative? lo) (- (find-between (- hi) (- lo))))
                                  (else (find-between lo hi))))))
                       (lambda (x within)
                         (check x) (check within)
                         (let* ((delta (abs within))
                                (lo (- x delta))
                                (hi (+ x delta)))
                           (cond
                            ((equal? x +nan.0) x)
                            ((or (equal? x +inf.0)
                                 (equal? x -inf.0))
                             (if (equal? delta +inf.0) +nan.0 x))
                            ((equal? delta +inf.0) 0.0)
                            ((not (= x x)) +nan.0)
                            ((<= lo 0 hi) (if (exact? x) 0 0.0))
                            ((or (inexact? lo) (inexact? hi))
                             (exact->inexact (do-find-between (inexact->exact lo) (inexact->exact hi))))
                            (else (do-find-between lo hi)))))))""",

            // FIXME naive memoize implementation
            """(define (memoize f)
                     (let ((mem (atom {})))
                       (fn args
                         (let ((e (find @mem args)))
                           (if e
                             (val e)
                             (let ((ret (apply f args)))
                               (swap! mem put args ret)
                             ret))))))""",

            "(define (negate f) (fn args (not (apply f args))))",
            "(define complement negate)",

            "(def (partial f . args1) (fn args2 (apply f (append args1 args2))))",
            "(def (const arg) (fn rest arg))",
            "(def (constantly arg) (fn rest arg))",

            // TODO Implement these in Kotlin?
            """(define (some pred coll)
                     (if (empty? coll)
                       null
                       (or (pred (first coll)) (some pred (rest coll)))))""",

            """(define (every? pred coll)
                     (cond
                      ((empty? coll) true)
                      ((pred (first coll)) (every? pred (rest coll)))
                      (else false))""",

            """(define (filter pred coll)
                     (lazy-seq
                       (let ((s (seq coll)))
                         (if s
                             (let ((f (first s))
                                   (r (rest s)))
                               (if (pred f)
                                   (cons-seq f (filter pred r))
                                   (filter pred r)))
                             '()))))""")
    }
}
