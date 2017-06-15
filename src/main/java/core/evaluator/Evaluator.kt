package core.evaluator

import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.exceptions.ReentrantContinuationException
import core.procedures.AFn
import core.procedures.continuations.CalledContinuation
import core.scm.*
import core.scm.specialforms.ISpecialForm
import core.scm.specialforms.New
import core.utils.Utils
import core.writer.Writer
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicLong

class Evaluator(private val reflector: Reflector = Reflector()) {

    companion object {
        /* Executor Service for Futures */
        private val threadCounter = AtomicLong(0)
        @Volatile var executor = Executors.newFixedThreadPool(2 + Runtime.getRuntime().availableProcessors(),
                createThreadFactory(threadCounter))!!

        private fun createThreadFactory(threadCounter: AtomicLong) = ThreadFactory { r ->
            val t = Thread(r)
            t.name = "executor-thread-${threadCounter.getAndIncrement()}"
            t
        }
    }

    inner class JavaMethodCall(val method: String) : AFn<Any?, Any?>(name = method) {
        override fun invoke(vararg args: Any?) = reflector.evalJavaMethod(method, args as Array<Any?>)
    }

    /* Macroexpand S-expression, evaluate it and then return the result */
    fun macroexpandAndEvaluate(sexp: Any, env: Environment) = eval(macroexpand(sexp), env)

    // TODO Implement
    private fun macroexpand(sexp: Any): Any {
        return sexp
    }

    /* Main eval */
    fun eval(sexp: Any?, env: Environment): Any? {
        var result: Any?
        try {
            /* TCO: This is our Trampoline */
            result = evalIter(sexp, env)
            while (result is Thunk) {
                result = evalIter(result.expr, result.context ?: env)
            }
        } catch (cc: CalledContinuation) {
            /* We have one-shot continuations only, not full continuations.
             * It means that we can't use the same continuation multiple times. */
            if (cc.continuation.isInvoked) throw ReentrantContinuationException()
            /* Continuation is still valid, rethrow it further (should be caught by callcc)  */
            throw cc
        }
        if (result is BigRatio) {
            return Utils.downcastNumber(result as Number)
        }
        return result
    }

    /**
     * One iteration of evaluation.
     * Returns the end result or a Thunk object.
     * If Thunk is returned, then eval() method (trampoline) continues evaluation.
     */
    private fun evalIter(sexp: Any?, env: Environment): Any? {
        return when (sexp) {
            is Symbol           -> sexp.eval(env)
            is MutableList<*>   -> sexp.eval(env)
            is Map<*, *>        -> sexp.eval(env)
            is Vector           -> sexp.eval(env)
            is Set<*>           -> sexp.eval(env)
            /* Everything else evaluates to itself: Numbers, Strings, Chars, Keywords etc. */
            else -> sexp
        }
    }

    /* Evaluate Symbol */
    private fun Symbol.eval(env: Environment): Any? {
        /* Check if it is a Special Form */
        val result = env.findOrDefault(this, Environment.UNDEFINED)
        if (result is ISpecialForm) throw IllegalSyntaxException.of(result.toString(), this)
        if (result === Environment.UNDEFINED) {
            /* Check if it is a Java class. If not found, then assume it is a static field */
            return reflector._getClass(name) ?: reflector.evalJavaStaticField(toString())
        }
        return result
    }

    /* Evaluate list */
    private fun List<Any?>.eval(env: Environment): Any? {
        if (isEmpty()) throw IllegalSyntaxException.of("eval", this, "illegal empty application")
        var op = this[0]
        if (op is Symbol) {
            val symbolName = op.name
            /* Lookup symbol */
            op = env.findOrDefault(op, Environment.UNDEFINED)
            /* Inline Special Forms and Pure functions
             * Doesn't help much, so commenting it out for now
             * if (op is ISpecialForm || (op is AFn<*, *> && op.isPure)) { this[0] = op } else */
            if (op === Environment.UNDEFINED) {
                // TODO implement as a macro
                /* Special case: constructor call If Symbol ends with . */
                if (symbolName.endsWith('.')) {
                    val clazz = Symbol.intern(symbolName.substring(0, symbolName.length - 1))
                    val form = mutableListOf<Any?>(New.NEW, clazz)
                    /* Add args (if any) */
                    (1..size - 1).forEach { i -> form.add(this[i]) }
                    return New.NEW.eval(form, env, this@Evaluator)
                }
                op = JavaMethodCall(this[0].toString())
            }
        }

        if (op is ISpecialForm) return op.eval(this, env, this@Evaluator)

        // TODO Do not evaluate if already evaluated?
        op = eval(op, env)
        /* If result is not a function, then raise an error */
        if (op !is AFn<*, *>) throw IllegalArgumentException("wrong type to apply: ${Writer.write(op)}")

        /* Scheme has applicative order, so evaluate all arguments first */
        val args = arrayOfNulls<Any>(size - 1)
        (1..size - 1).forEach { i -> args[i - 1] = eval(this[i], env) }
        /* Call AFn via helper method */
        return (op as AFn<Any?, Any?>).invokeN(*args)
    }

    /* Evaluate hash map */
    private fun Map<*, *>.eval(env: Environment): Map<*, *> {
        val result = InvokableMap(size)
        forEach { (key, value) -> result.put(eval(key, env), eval(value, env)) }
        return result
    }

    /* Evaluate vector */
    private fun Vector.eval(env: Environment): Vector {
        indices.forEach { i -> array[i] = eval(array[i], env) }
        return this
    }

    /* Evaluate set */
    private fun Set<Any?>.eval(env: Environment) = mapTo(HashSet<Any?>(size)) { eval(it, env) }
}
