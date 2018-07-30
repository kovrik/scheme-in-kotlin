package core.procedures.predicates

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity
import core.procedures.Arity.Exactly
import core.procedures.IFn
import core.procedures.math.Remainder
import core.scm.*
import core.scm.Vector
import core.utils.Utils
import java.math.BigDecimal
import java.util.*
import java.util.concurrent.CompletableFuture

class Predicate private constructor(override val name: String, private inline val predicate: (Any?) -> Boolean) :
        AFn<Any?, Boolean>(isPure = true, arity = Exactly(1)) {

    companion object {
        val IS_NULL = Predicate("null?", Objects::isNull)
        val IS_NIL = Predicate("nil?", Objects::isNull)
        val IS_EOF = Predicate("eof-object?", Objects::isNull)
        val IS_SOME = Predicate("some?", Objects::nonNull)
        val IS_EMPTY = Predicate("empty?", Utils::isEmpty)
        val IS_PAIR = Predicate("pair?", this::isPair)
        val IS_MPAIR = Predicate("mpair?") { it is MutablePair<*, *> }
        val IS_LIST = Predicate("list?", this::isProperList)
        val IS_PROMISE = Predicate("promise?") { it is CompletableFuture<*> || Delay::class.java == it!!.javaClass }
        val IS_FUTURE = Predicate("future?") { it is java.util.concurrent.Future<*> && it !is Delay && it !is Promise }
        val IS_FUTURE_DONE = Predicate("future-done?") { Type.assertType("future-done?", it, java.util.concurrent.Future::class.java) && (it as java.util.concurrent.Future<*>).isDone }
        val IS_FUTURE_CANCELLED = Predicate("future-cancelled?") { Type.assertType("future-cancelled?", it, java.util.concurrent.Future::class.java) && (it as java.util.concurrent.Future<*>).isCancelled }
        val IS_DELAY = Predicate("delay?") { it is Delay }
        val IS_REALIZED = Predicate("realized?", this::isRealized)
        val IS_DELAY_FORCED = Predicate("delay-forced?") { it is Delay && it.isForced()}
        val IS_CHAR = Predicate("char?") { it is Char }
        val IS_STRING = Predicate("string?") { it is CharSequence }
        val IS_VECTOR = Predicate("vector?") { it is Vector }
        val IS_SET = Predicate("set?") { it is Set<*> }
        val IS_MAP = Predicate("map?") { it is Map<*, *> }
        val IS_MAP_ENTRY = Predicate("map-entry?") { it is Map.Entry<*, *> }
        val IS_COLL = Predicate("coll?") { it is Collection<*> || it is Map<*, *> || it is Sequence<*> }
        val IS_SYMBOL = Predicate("symbol?") { it is Symbol }
        val IS_BOOLEAN = Predicate("boolean?") { it is Boolean }
        val IS_TRUE = Predicate("true?") { it is Boolean && it }
        val IS_FALSE = Predicate("false?") { it is Boolean && !it }
        val IS_PORT = Predicate("port?") { it is IPort }
        val IS_INPUT_PORT = Predicate("input-port?") { it is InputPort }
        val IS_OUTPUT_PORT = Predicate("output-port?") { it is OutputPort }
        val IS_NUMBER = Predicate("number?") { it is Number }
        val IS_INTEGER = Predicate("integer?", Utils::isInteger)
        val IS_EXACT_INTEGER = Predicate("exact-integer?", Utils::isExactInteger)
        val IS_RATIONAL = Predicate("rational?", Utils::isRational)
        val IS_RATIO = Predicate("ratio?") { it is BigRatio }
        val IS_REAL = Predicate("real?", Utils::isReal)
        val IS_COMPLEX = Predicate("complex?") { it is Number }
        val IS_ZERO = Predicate("zero?") { Type.assertType("zero?", it, Number::class.java) && Utils.isZero(it) }
        val IS_POSITIVE = Predicate("positive?") { Type.assertType("positive?", it, Type.Real::class.java) && Utils.isPositive(it) }
        val IS_POS = Predicate("pos?") { Type.assertType("pos?", it, Type.Real::class.java) && Utils.isPositive(it) }
        val IS_NEGATIVE = Predicate("negative?") { Type.assertType("negative?", it, Type.Real::class.java) && Utils.isNegative(it) }
        val IS_NEG = Predicate("neg?") { Type.assertType("neg?", it, Type.Real::class.java) && Utils.isNegative(it) }
        val IS_EXACT = Predicate("exact?") { Type.assertType("exact?", it, Number::class.java) && Utils.isExact(it) }
        val IS_INEXACT = Predicate("inexact?") { Type.assertType("inexact?", it, Number::class.java) && Utils.isInexact(it) }
        val IS_IMMUTABLE = Predicate("immutable?", this::isImmutable)
        val IS_MUTABLE = Predicate("mutable?", this::isMutable)
        val IS_EVEN = Predicate("even?") { Type.assertType("even?", it, Int::class.javaObjectType) && Utils.isZero(remainder(it as Number, 2L)) }
        val IS_ODD = Predicate("odd?") { Type.assertType("odd?", it, Int::class.javaObjectType) && !Utils.isZero(remainder(it as Number, 2L)) }
        val IS_NAN = Predicate("nan?") { Type.assertType("nan?", it, Type.Real::class.java) && Utils.isNaN(it as Number?) }
        val IS_FINITE = Predicate("finite?") { Type.assertType("finite?", it, Type.Real::class.java) && Utils.isFinite(it as Number?) }
        val IS_INFINITE = Predicate("infinite?") { Type.assertType("infinite?", it, Type.Real::class.java) && (Utils.isPositiveInfinity(it as Number?) || Utils.isNegativeInfinity(it)) }
        val IS_EXACT_NONNEGATIVE_INTEGER = Predicate("exact-nonnegative-integer?") { Utils.isExactNonNegativeInteger(it) }
        val IS_NATURAL = Predicate("natural?") { Utils.isExactNonNegativeInteger(it) }
        val IS_POSITIVE_INTEGER = Predicate("positive-integer?") { Utils.isInteger(it) && Utils.isPositive(it) }
        val IS_NEGATIVE_INTEGER = Predicate("negative-integer?") { Utils.isInteger(it) && Utils.isNegative(it) }
        val IS_NONPOSITIVE_INTEGER = Predicate("nonpositive-integer?") { Utils.isInteger(it) && !Utils.isPositive(it) }
        val IS_NONNEGATIVE_INTEGER = Predicate("nonnegative-integer?") { Utils.isInteger(it) && !Utils.isNegative(it) }
        val IS_KEYWORD = Predicate("keyword?") { it is Keyword }
        val IS_ANY = Predicate("any?") { true }
        val IS_BLANK = Predicate("blank?") { Type.assertType("blank?", it, String::class.java) && it == null || it!!.toString().isEmpty() || it.toString().trim { it <= ' ' }.isEmpty() }
        val IS_CLASS = Predicate("class?") { it is Class<*> }
        val IS_DECIMAL = Predicate("decimal?") { it is BigDecimal }
        val IS_FLOAT = Predicate("float?") { it is Float || it is Double }
        val IS_PROC = Predicate("procedure?", this::isProcedure)
        val IS_FN = Predicate("fn?", this::isProcedure)
        val IS_IFN = Predicate("ifn?") { it is IFn<*, *> }
        val IS_BOX = Predicate("box?") { it is Box<*> }
        val IS_WEAK_BOX = Predicate("weak-box?") { it is WeakBox<*> }
        val IS_ATOM = Predicate("atom?") { it is Box<*> }
        val IS_BYTE = Predicate("byte?", Utils::isByte)
        val IS_ARRAY = Predicate("array?") { it != null && it::class.java.isArray }
        val IS_BOOLEANS = Predicate("booleans?") { it is BooleanArray }
        val IS_BYTES = Predicate("bytes?") { it is ByteArray }
        val IS_CHARS = Predicate("chars?") { it is CharArray }
        val IS_DOUBLES = Predicate("doubles?") { it is DoubleArray }
        val IS_FLOATS = Predicate("floats?") { it is FloatArray }
        val IS_INTS = Predicate("ints?") { it is IntArray }
        val IS_LONGS = Predicate("longs?") { it is LongArray }
        val IS_SHORTS = Predicate("shorts?") { it is ShortArray }
        val IS_OBJECTS = Predicate("objects?") { it is Array<*> }
        val IS_VOID = Predicate("void?") { it === Unit }
        val IS_THREAD = Predicate("thread?") { it is Thread }
        val IS_THREAD_RUNNING = Predicate("thread-running?") { Type.assertType("thread-running?", it, Thread::class.java) && (it as Thread).isAlive }
        val IS_THREAD_DEAD = Predicate("thread-dead?") { Type.assertType("thread-dead?", it, Thread::class.java) && !(it as Thread).isAlive }
        val IS_SYNTAX = Predicate("syntax?") { it is Syntax }
        val IS_SEQUENCE = Predicate("sequence?") { it is Sequence<*> }
        val IS_SEQ = Predicate("seq?", Utils::isSeqable)
        val IS_SEQABLE = Predicate("seqable?", Utils::isSeqable)
        val IS_ARITY = Predicate("arity?") { it is Arity }

        private fun isMutable(o: Any?) = !isImmutable(o)

        private fun isImmutable(o: Any?) = when {
            Pair::class.java    == o?.javaClass -> true
            Vector::class.java  == o?.javaClass -> true
            String::class.java  == o?.javaClass -> true
            Hashmap::class.java == o?.javaClass -> true
            else -> false
        }

        private fun isRealized(o: Any?) = when (o) {
            is LazySeq -> o.realized
            is java.util.concurrent.Future<*> -> o.isDone
            else -> throw WrongTypeException("realized?", "Delay or Promise or Future or LazySeq", o)
        }

        private fun isProcedure(o: Any?) = o is IFn<*, *>  && o !is Symbol && o !is Keyword &&
                                           o !is Map<*, *> && o !is Vector && o !is Map.Entry<*, *>

        private val remainder = Remainder()

        fun isPair(o: Any?) = o is Pair<*, *> || o is List<*> && !o.isEmpty()

        /* Return true if o is a Proper List */
        fun isProperList(o: Any?) = when (o) {
            is Pair<*, *> -> false
            is MutablePair<*, *> -> false
            else -> o is List<*> && o !is Cons<*> || o is Cons<*> && o.isProperList || o is Sequence<*>
        }
    }

    override operator fun invoke(arg: Any?) = predicate(arg)
}
