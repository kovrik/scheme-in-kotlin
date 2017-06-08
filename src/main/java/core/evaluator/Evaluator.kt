package core.evaluator

import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.exceptions.ReentrantContinuationException
import core.procedures.AFn
import core.procedures.FnArgs
import core.procedures.continuations.CalledContinuation
import core.scm.*
import core.scm.Vector
import core.scm.specialforms.ISpecialForm
import core.scm.specialforms.New
import core.utils.Utils
import core.writer.Writer
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicLong

class Evaluator {

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

    private val reflector = Reflector()

    inner class JavaMethodCall(val method: String) : AFn() {
        override val name = method
        override fun invoke(vararg args: Any?) = reflector.evalJavaMethod(method, args as Array<Any?>)
    }

    // TODO Use custom HashMap class that extends AFn instead
    inner class InvokableMap(val map: Map<Any?, Any?>) : AFn(FnArgs(min = 1, max = 2)) {
        /* Maps are functions of their keys */
        override fun invoke(vararg args: Any?) = when (args.size) {
            1    -> map[args[0]]
            else -> map.getOrDefault(args[0], args[1])
        }
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
            is MutableList<*>   -> (sexp as MutableList<Any?>).eval(env)
            is MutableMap<*, *> -> (sexp as Map<Any?, Any?>).eval(env)
            is Vector           -> sexp.eval(env)
            is MutableSet<*>    -> sexp.eval(env)
            /* Everything else evaluates to itself: Numbers, Strings, Chars, Keywords etc. */
            else -> sexp
        }
    }

    /* Evaluate Symbol */
    private fun Symbol.eval(env: Environment): Any? {
        /* Check if it is a Special Form */
        val o = env.findOrDefault(this, Environment.UNDEFINED)
        if (o is ISpecialForm) throw IllegalSyntaxException.of(o.toString(), this)
        if (o === Environment.UNDEFINED) {
            /* Check if it is a Java class. If not found, then assume it is a static field */
            return reflector._getClass(name) ?: reflector.evalJavaStaticField(toString())
        }
        return o
    }

    /* Evaluate list */
    private fun MutableList<Any?>.eval(env: Environment): Any? {
        if (isEmpty()) throw IllegalSyntaxException.of("eval", this, "illegal empty application")
        var op = this[0]
        if (op is Symbol) {
            val symbolName = op.name
            /* Lookup symbol */
            op = env.findOrDefault(op, Environment.UNDEFINED)
            /* Inline Special Forms and Pure functions */
            if (op is ISpecialForm || (op is AFn && op.isPure)) {
                this[0] = op
            } else if (op === Environment.UNDEFINED) {
                /* Special case: constructor call If Symbol ends with . */
                if (symbolName.endsWith('.')) {
                    this[0] = Symbol.intern(symbolName.substring(0, symbolName.length - 1))
                    op = New.NEW
                    (this as Cons<Any>).push(op)
                } else {
                    op = JavaMethodCall(this[0].toString())
                }
            }
        }

        /* If it is a Special Form, then evaluate it */
        if (op is ISpecialForm) return op.eval(this, env, this@Evaluator)

        /* If it is not AFn, then try to evaluate it (assuming it is a Lambda) */
        if (op !is AFn) op = eval(op, env)

        /* Vectors and Map Entries are functions of index */
        when (op) {
            is Vector                -> op = eval(op, env)
            is Map<*, *>             -> op = InvokableMap(op as Map<Any?, Any?>)
            is Map.Entry<Any?, Any?> -> op = MapEntry(op)
        }
        /* If result is not a function, then raise an error */
        if (op !is AFn) throw IllegalArgumentException("wrong type to apply: ${Writer.write(op)}")

        /* Scheme has applicative order, so evaluate all arguments first */
        val args = arrayOfNulls<Any>(size - 1)
        for (i in 1..size - 1) {
            args[i - 1] = eval(this[i], env)
        }
        /* Call AFn via helper method */
        return op.invokeN(*args)
    }

    /* Evaluate hash map */
    private fun Map<Any?, Any?>.eval(env: Environment): Map<Any?, Any?> {
        val result = HashMap<Any?, Any?>(size)
        for ((key, value) in this) {
            result.put(eval(key, env), eval(value, env))
        }
        return result
    }

    /* Evaluate vector */
    private fun Vector.eval(env: Environment): Vector {
        for (i in indices) {
            array[i] = eval(array[i], env)
        }
        return this
    }

    /* Evaluate set */
    private fun Set<Any?>.eval(env: Environment): Set<Any?> {
        val result = HashSet<Any?>(size)
        this.mapTo(result) { eval(it, env) }
        return result
    }
}
