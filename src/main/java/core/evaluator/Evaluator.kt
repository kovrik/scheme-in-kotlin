package core.evaluator

import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.exceptions.ReentrantContinuationException
import core.procedures.AFn
import core.procedures.IFn
import core.scm.*
import core.scm.specialforms.ISpecialForm
import core.scm.specialforms.New
import core.utils.Utils
import core.writer.Writer
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicLong

class Evaluator(private val reflector: Reflector = Reflector(),
                private val macroexpander: Macroexpander = Macroexpander()) {

    companion object {
        /* Executor Service for Futures */
        private val threadCounter = AtomicLong(0)
        @Volatile var executor = Executors.newFixedThreadPool(2 + Runtime.getRuntime().availableProcessors(),
                { Thread(it, "executor-thread-${threadCounter.getAndIncrement()}") }
        )
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
    private fun Symbol.eval(env: Environment) = env.findOrDefault(this, Environment.UNDEFINED).let {
        when (it) {
            is ISpecialForm -> throw IllegalSyntaxException.of(it.toString(), this)
            /* Check if it is a Java class. If not found, then assume it is a static field */
            Environment.UNDEFINED -> reflector.getClazzOrNull(name) ?: reflector.evalJavaStaticField(toString())
            else -> it
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
                        val form = mutableListOf<Any?>(New, Symbol.intern(symbolName.dropLast(1)))
                        for (i in 1..size - 1) { form.add(this[i]) }
                        return New.eval(form, env, this@Evaluator)
                    }
                    op = JavaMethodCall(this[0].toString())
                }
            }
        }
        /* Now decide how to evaluate everything else */
        return when (op) {
            /* Special Forms have special evaluation rules */
            is ISpecialForm -> op.eval(this, env, this@Evaluator)
            /* Op is a valid invokable object (procedure)
             * Scheme has applicative order, so evaluate all arguments first
             * and then invoke operator (IFn) via helper method */
            is IFn<*, *> -> AFn.invokeN(op, arrayOfNulls<Any>(size - 1).apply {
                for (i in 0..size - 1) { set(i, eval(this@eval[i + 1], env)) }
            })
            /* If operator is not invokable, then raise an error */
            else -> throw IllegalArgumentException("wrong type to apply: ${Writer.write(op)}")
        }
    }

    /* Evaluate hash map */
    private fun Map<*, *>.eval(env: Environment) = Hashmap(size).apply { this@eval.forEach { k, v -> put(eval(k, env), eval(v, env)) } }

    /* Evaluate vector */
    private fun Vector.eval(env: Environment) = apply { indices.forEach { array[it] = eval(array[it], env) } }

    /* Evaluate set */
    private fun Set<Any?>.eval(env: Environment) = mapTo(HashSet<Any?>(size)) { eval(it, env) }
}
