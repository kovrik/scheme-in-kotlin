package core.evaluator

import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.exceptions.ReentrantContinuationException
import core.procedures.AFn
import core.procedures.IFn
import core.procedures.continuations.CalledContinuation
import core.scm.*
import core.scm.specialforms.ISpecialForm
import core.scm.specialforms.New
import core.utils.Utils
import core.writer.Writer
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicLong

class Evaluator(private val reflector: Reflector = Reflector(),
                private val macroexpander: Macroexpander = Macroexpander()) {

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
        override fun invoke(args: Array<out Any?>) = reflector.evalJavaMethod(method, args)
    }

    /* Macroexpand S-expression, evaluate it and then return the result */
    fun macroexpandAndEvaluate(sexp: Any, env: Environment) = eval(macroexpander.expand(sexp), env)

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
        return when (result) {
            is BigRatio -> Utils.downcastNumber(result)
            else        -> result
        }
    }

    /**
     * One iteration of evaluation.
     * Returns the end result or a Thunk object.
     * If Thunk is returned, then eval() method (trampoline) continues evaluation.
     */
    private fun evalIter(sexp: Any?, env: Environment) = when (sexp) {
        is Symbol    -> sexp.eval(env)
        is List<*>   -> sexp.eval(env)
        is Map<*, *> -> sexp.eval(env)
        is Vector    -> sexp.eval(env)
        is Set<*>    -> sexp.eval(env)
        else         -> sexp
    }

    /* Evaluate Symbol */
    private fun Symbol.eval(env: Environment): Any? {
        val result = env.findOrDefault(this, Environment.UNDEFINED)
        return when {
            result is ISpecialForm -> throw IllegalSyntaxException.of(result.toString(), this)
            /* Check if it is a Java class. If not found, then assume it is a static field */
            result === Environment.UNDEFINED -> reflector._getClass(name) ?: reflector.evalJavaStaticField(toString())
            else -> result
        }
    }

    /* Evaluate list */
    private fun List<Any?>.eval(env: Environment): Any? {
        if (isEmpty()) throw IllegalSyntaxException.of("eval", this, "illegal empty application")
        var op = this[0]
        /* Evaluate operator */
        when (op) {
            is List<*>, is Map<*, *>, is Vector, is Set<*> -> op = eval(op, env)
            is Symbol -> {
                /* Lookup symbol */
                op = env.findOrDefault(op, Environment.UNDEFINED)
                /* Inline Special Forms and Pure functions
                 * Doesn't help much, so commenting it out for now
                 * if (op is ISpecialForm || (op is AFn<*, *> && op.isPure)) { this[0] = op } else */
                if (op === Environment.UNDEFINED) {
                    // TODO implement as a macro
                    /* Special case: constructor call If Symbol ends with . */
                    val symbolName = (this[0] as Symbol).name
                    if (symbolName.endsWith('.')) {
                        val clazz = Symbol.intern(symbolName.substring(0, symbolName.length - 1))
                        val form = mutableListOf<Any?>(New, clazz)
                        /* Add args (if any) */
                        for (i in 1..size - 1) { form.add(this[i]) }
                        return New.eval(form, env, this@Evaluator)
                    }
                    op = JavaMethodCall(this[0].toString())
                }
            }
        }
        /* Now decide how to evaluate everything else */
        when (op) {
            /* Special Forms have special evaluation rules */
            is ISpecialForm -> return op.eval(this, env, this@Evaluator)
            /* Operator is a valid invokable object */
            is IFn<*, *> -> {
                /* Scheme has applicative order, so evaluate all arguments first */
                val args = arrayOfNulls<Any>(size - 1)
                for (it in 1..args.size) { args[it - 1] = eval(this[it], env) }
                /* Finally, invoke operator (IFn) via helper method */
                return AFn.invokeN(op, args)
            }
            /* If operator is not invokable, then raise an error */
            else -> throw IllegalArgumentException("wrong type to apply: ${Writer.write(op)}")
        }
    }

    /* Evaluate hash map */
    private fun Map<*, *>.eval(env: Environment): Map<*, *> {
        val result = Hashmap(size)
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
