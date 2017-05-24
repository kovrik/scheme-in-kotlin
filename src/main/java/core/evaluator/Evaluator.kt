package core.evaluator

import core.environment.Environment
import core.exceptions.ArityException
import core.exceptions.IllegalSyntaxException
import core.exceptions.ReentrantContinuationException
import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.continuations.CalledContinuation
import core.scm.*
import core.scm.Vector
import core.scm.specialforms.ISpecialForm
import core.scm.specialforms.New
import core.utils.Utils
import core.writer.Writer
import java.util.*
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicLong

class Evaluator {

    companion object {

        /* Executor Service for Futures */
        private val threadCounter = AtomicLong(0)
        @Volatile var executor = Executors.newFixedThreadPool(2 + Runtime.getRuntime().availableProcessors(),
                createThreadFactory(threadCounter))

        private fun createThreadFactory(threadCounter: AtomicLong): ThreadFactory {
            return ThreadFactory { r ->
                val t = Thread(r)
                t.name = "executor-thread-" + threadCounter.getAndIncrement()
                t
            }
        }
    }

    private val reflector = Reflector()

    /* Macroexpand S-expression, evaluate it and then return the result */
    fun macroexpandAndEvaluate(sexp: Any, env: Environment): Any? {
        return eval(macroexpand(sexp), env)
    }

    // TODO Implement
    private fun macroexpand(sexp: Any): Any {
        return sexp
    }

    fun eval(sexp: Any?, env: Environment): Any? {
        /* TCO: This is our Trampoline */
        var result: Any?
        try {
            result = evalIter(sexp, env)
            while (result is Thunk) {
                result = evalIter(result.expr, result.getContextOrDefault(env))
            }
        } catch (cc: CalledContinuation) {
            if (cc.continuation.isInvoked) {
                /* We have one-shot continuations only, not full continuations.
         * It means that we can't use the same continuation multiple times. */
                throw ReentrantContinuationException()
            }
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
        if (sexp is Symbol) {
            return evalSymbol(sexp, env)
        } else if (sexp is MutableList<*>) {
            return evlis(sexp as MutableList<Any?>, env)
        } else if (sexp is MutableMap<*, *>) {
            return evalMap(sexp as MutableMap<Any, Any>, env)
        } else if (sexp is Vector) {
            return evalVector(sexp, env)
        } else if (sexp is MutableSet<*>) {
            return evalSet(sexp as MutableSet<Any>, env)
        } else {
            /* Everything else evaluates to itself:
       * Numbers, Strings, Chars, Keywords etc. */
            return sexp
        }
    }

    /* Evaluate Symbol */
    private fun evalSymbol(symbol: Symbol, env: Environment): Any? {
        /* Check if it is a Special Form */
        val o = env.findOrDefault(symbol, Environment.UNDEFINED)
        if (o is ISpecialForm) {
            throw IllegalSyntaxException.of(o.toString(), symbol)
        }
        if (o === Environment.UNDEFINED) {
            /* Check if it is a Java class. If not found, then assume it is a static field */
            val clazz = reflector._getClass(symbol.name)
            return clazz ?: reflector.evalJavaStaticField(symbol.toString())
        }
        return o
    }

    /* Evaluate list */
    private fun evlis(sexp: MutableList<Any?>, env: Environment): Any? {
        if (sexp.isEmpty()) {
            throw IllegalSyntaxException.of("eval", sexp, "illegal empty application")
        }

        var javaMethod = false
        var op: Any? = sexp[0]
        if (op is Symbol) {
            val sym = op
            /* Lookup symbol */
            op = env.findOrDefault(sym, Environment.UNDEFINED)
            /* Inline Special Forms and Pure functions */
            if (op is ISpecialForm) {
                sexp[0] = op
            } else if (op is AFn) {
                if (op.isPure) {
                    sexp[0] = op
                }
            } else if (op === Environment.UNDEFINED) {
                // TODO Check if op starts with '.' instead?
                javaMethod = true
                /* Special case: constructor call If Symbol ends with . */
                if (sym.name[sym.name.length - 1] == '.') {
                    // TODO Optimize and cleanup
                    sexp[0] = Symbol.intern(sym.name.substring(0, sym.name.length - 1))
                    op = New.NEW
                    (sexp as Cons<Any>).push(op)
                }
            }
        }

        /* If it is a Special Form, then evaluate it */
        if (op is ISpecialForm) {
            return op.eval(sexp, env, this)
        }

        /* If it is not AFn, then try to evaluate it (assuming it is a Lambda) */
        if (op !is AFn) {
            op = eval(op, env)
        }

        /* Vectors and Map Entries are functions of index */
        if (op is Map.Entry<Any?, Any?>) {
            /* Convert Map Entry into a MapEntry */
            op = MapEntry(op)
        }
        if (op is Vector) {
            if (sexp.size > 2) throw ArityException("vector", 1, 1, sexp.size - 1)
            val vector = evalVector(op, env)
            /* Evaluate key */
            val key = eval(sexp[1], env)
            if (!Utils.isInteger(key)) {
                throw WrongTypeException("vector", Int::class.java, key)
            }
            val i = (key as Number).toInt()
            if (vector.size <= i || i < 0) {
                throw IndexOutOfBoundsException(String.format("%s: value out of range: %s", vector, i))
            }
            return vector[i]
        } else if (op is Map<*, *>) {
            /* Maps are functions of their keys */
            if (sexp.size > 3) throw ArityException("hashmap", 1, 2, sexp.size - 1)
            val map = evalMap(op as Map<Any, Any>, env)
            /* Evaluate key */
            val key = eval(sexp[1], env)
            val defaultValue = if (sexp.size == 3) eval(sexp[2], env) else null
            return map.getOrDefault(key, defaultValue)
        }

        /* If result is not a function, then raise an error */
        if (op !is AFn && !javaMethod) {
            throw IllegalArgumentException("wrong type to apply: " + Writer.write(op))
        }

        /* Scheme has applicative order, so evaluate all arguments first */
        val args = arrayOfNulls<Any>(sexp.size - 1)
        for (i in 1..sexp.size - 1) {
            args[i - 1] = eval(sexp[i], env)
        }

        /* Call Java method */
        if (javaMethod) {
            val method = sexp[0].toString()
            return reflector.evalJavaMethod(method, args)
        }
        /* Call AFn via helper method */
        return (op as AFn).applyN(args)
    }

    /* Evaluate hash map */
    private fun evalMap(map: Map<Any, Any>, env: Environment): Map<Any?, Any?> {
        val result = HashMap<Any?, Any?>(map.size)
        for ((key1, value1) in map) {
            val key = eval(key1, env)
            val value = eval(value1, env)
            result.put(key, value)
        }
        return result
    }

    /* Evaluate vector */
    private fun evalVector(vector: Vector, env: Environment): Vector {
        for (i in vector.indices) {
            vector.array[i] = eval(vector.array[i], env)
        }
        return vector
    }

    /* Evaluate set */
    private fun evalSet(set: Set<Any>, env: Environment): Set<Any?> {
        val result = HashSet<Any?>(set.size)
        for (e in set) {
            result.add(eval(e, env))
        }
        return result
    }
}
