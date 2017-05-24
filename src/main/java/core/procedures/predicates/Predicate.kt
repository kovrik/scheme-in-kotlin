package core.procedures.predicates

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.procedures.IFn
import core.procedures.math.Remainder
import core.scm.*
import core.scm.Vector
import core.utils.Utils
import java.math.BigDecimal
import java.util.*
import java.util.concurrent.CompletableFuture

class Predicate private constructor(override val name: String, private val predicate: java.util.function.Predicate<Any?>) : AFn(FnArgsBuilder().min(1).max(1).build()) {

    companion object {
        val IS_NULL = Predicate("null?", java.util.function.Predicate<Any?> { Objects.isNull(it) })
        val IS_NIL = Predicate("nil?", java.util.function.Predicate<Any?> { Objects.isNull(it) })
        val IS_EOF = Predicate("eof-object?", java.util.function.Predicate<Any?> { Objects.isNull(it) })
        val IS_SOME = Predicate("some?", java.util.function.Predicate<Any?> { Objects.nonNull(it) })
        val IS_EMPTY = Predicate("empty?", java.util.function.Predicate<Any?> { o -> o == null || isEmpty(o) })
        val IS_PAIR = Predicate("pair?", java.util.function.Predicate<Any?> { Cons.isPair(it) })
        val IS_LIST = Predicate("list?", java.util.function.Predicate<Any?> { Cons.isList(it) })
        val IS_PROMISE = Predicate("promise?", java.util.function.Predicate<Any?> { o -> o is CompletableFuture<*> || Delay::class.java == o!!.javaClass })
        val IS_FUTURE = Predicate("future?", java.util.function.Predicate<Any?> { o -> o is java.util.concurrent.Future<*> && o !is Delay && o !is Promise })
        val IS_FUTURE_DONE = Predicate("future-done?", java.util.function.Predicate<Any?> { o -> assertClass("future-done?", o, java.util.concurrent.Future::class.java) && (o as java.util.concurrent.Future<*>).isDone })
        val IS_FUTURE_CANCELLED = Predicate("future-cancelled?", java.util.function.Predicate<Any?> { o -> assertClass("future-cancelled?", o, java.util.concurrent.Future::class.java) && (o as java.util.concurrent.Future<*>).isCancelled })
        val IS_DELAY = Predicate("delay?", java.util.function.Predicate<Any?> { o -> o is Delay })
        val IS_REALIZED = Predicate("realized?", java.util.function.Predicate<Any?> { isRealized(it) })
        val IS_CHAR = Predicate("char?", java.util.function.Predicate<Any?> { o -> o is Char })
        val IS_STRING = Predicate("string?", java.util.function.Predicate<Any?> { o -> o is CharSequence })
        val IS_VECTOR = Predicate("vector?", java.util.function.Predicate<Any?> { o -> o is Vector })
        val IS_SET = Predicate("set?", java.util.function.Predicate<Any?> { o -> o is Set<*> })
        val IS_MAP = Predicate("map?", java.util.function.Predicate<Any?> { o -> o is Map<*, *> })
        val IS_MAP_ENTRY = Predicate("map-entry?", java.util.function.Predicate<Any?> { o -> o is Map.Entry<*, *> })
        val IS_COLL = Predicate("coll?", java.util.function.Predicate<Any?> { o -> o is Collection<*> || o is Map<*, *> })
        val IS_SYMBOL = Predicate("symbol?", java.util.function.Predicate<Any?> { o -> o is Symbol })
        val IS_BOOLEAN = Predicate("boolean?", java.util.function.Predicate<Any?> { o -> o is Boolean })
        val IS_TRUE = Predicate("true?", java.util.function.Predicate<Any?> { o -> o is Boolean && o })
        val IS_FALSE = Predicate("false?", java.util.function.Predicate<Any?> { o -> o is Boolean && !o })
        val IS_PROC = Predicate("procedure?", java.util.function.Predicate<Any?> { isProcedure(it) })
        val IS_PORT = Predicate("port?", java.util.function.Predicate<Any?> { o -> o is IPort })
        val IS_INPUT_PORT = Predicate("input-port?", java.util.function.Predicate<Any?> { o -> o is InputPort })
        val IS_OUTPUT_PORT = Predicate("output-port?", java.util.function.Predicate<Any?> { o -> o is OutputPort })
        val IS_NUMBER = Predicate("number?", java.util.function.Predicate<Any?> { o -> o is Number })
        val IS_INTEGER = Predicate("integer?", java.util.function.Predicate<Any?> { Utils.isInteger(it) })
        val IS_RATIONAL = Predicate("rational?", java.util.function.Predicate<Any?> { Utils.isRational(it) })
        val IS_RATIO = Predicate("ratio?", java.util.function.Predicate<Any?> { o -> o is BigRatio })
        val IS_REAL = Predicate("real?", java.util.function.Predicate<Any?> { Utils.isReal(it) })
        val IS_COMPLEX = Predicate("complex?", java.util.function.Predicate<Any?> { o -> o is Number })
        val IS_ZERO = Predicate("zero?", java.util.function.Predicate<Any?> { o -> assertClass("zero?", o, Number::class.java) && Utils.isZero(o) })
        val IS_POSITIVE = Predicate("positive?", java.util.function.Predicate<Any?> { o -> assertClass("positive?", o, Type.Real::class.java) && Utils.isPositive(o) })
        val IS_POS = Predicate("pos?", java.util.function.Predicate<Any?> { o -> assertClass("pos?", o, Type.Real::class.java) && Utils.isPositive(o) })
        val IS_NEGATIVE = Predicate("negative?", java.util.function.Predicate<Any?> { o -> assertClass("negative?", o, Type.Real::class.java) && Utils.isNegative(o) })
        val IS_NEG = Predicate("neg?", java.util.function.Predicate<Any?> { o -> assertClass("neg?", o, Type.Real::class.java) && Utils.isNegative(o) })
        val IS_EXACT = Predicate("exact?", java.util.function.Predicate<Any?> { o -> assertClass("exact?", o, Number::class.java) && Utils.isExact(o) })
        val IS_INEXACT = Predicate("inexact?", java.util.function.Predicate<Any?> { o -> assertClass("inexact?", o, Number::class.java) && Utils.isInexact(o) })
        val IS_IMMUTABLE = Predicate("immutable?", java.util.function.Predicate<Any?> { isImmutable(it) })
        val IS_MUTABLE = Predicate("mutable?", java.util.function.Predicate<Any?> { isMutable(it) })
        val IS_EVEN = Predicate("even?", java.util.function.Predicate<Any?> { o -> assertClass("even?", o, Int::class.javaObjectType) && Utils.isZero(Remainder.remainder(o as Number, 2L)) })
        val IS_ODD = Predicate("odd?", java.util.function.Predicate<Any?> { o -> assertClass("odd?", o, Int::class.javaObjectType) && !Utils.isZero(Remainder.remainder(o as Number, 2L)) })
        val IS_KEYWORD = Predicate("keyword?", java.util.function.Predicate<Any?> { o -> o is Keyword })
        val IS_ANY = Predicate("any?", java.util.function.Predicate<Any?> { _ -> true })
        val IS_BLANK = Predicate("blank?", java.util.function.Predicate<Any?> { o -> assertClass("blank?", o, String::class.java) && o == null || o!!.toString().isEmpty() || o.toString().trim({ it <= ' ' }).isEmpty() })
        val IS_CLASS = Predicate("class?", java.util.function.Predicate<Any?> { o -> o is Class<*> })
        val IS_DECIMAL = Predicate("decimal?",java.util.function.Predicate<Any?>  { o -> o is BigDecimal })
        val IS_FLOAT = Predicate("float?", java.util.function.Predicate<Any?> { o -> o is Float || o is Double })
        val IS_FN = Predicate("fn?", java.util.function.Predicate<Any?> { isProcedure(it) })

        private fun isMutable(o: Any?): Boolean {
            return !isImmutable(o)
        }

        private fun isImmutable(o: Any?): Boolean {
            return !(o is MutableString || o is MutableVector)
        }

        private fun assertClass(name: String, o: Any?, c: Class<*>): Boolean {
            if (Type.checkType(o, c)) {
                return true
            }
            throw WrongTypeException(name, c, o)
        }

        private fun isEmpty(o: Any?): Boolean {
            when (o) {
                is Collection<*> -> return o.isEmpty()
                is CharSequence  -> return o.length == 0
                is Map<*, *>     -> return o.size == 0
                else             -> return false
            }
        }

        private fun isRealized(o: Any?): Boolean {
            if (o is java.util.concurrent.Future<*>) {
                return o.isDone
            }
            throw WrongTypeException("realized?", "Delay or Promise or Future", o)
        }

        private fun isProcedure(o: Any?): Boolean {
            return o is IFn<*, *> && o !is Symbol && o !is Keyword && o !is Map<*, *> &&
                   o !is Vector && o !is Map.Entry<*, *>
        }
    }

    override val isPure: Boolean
        get() = true

    override fun apply1(arg: Any?): Boolean {
        return predicate.test(arg)
    }
}
