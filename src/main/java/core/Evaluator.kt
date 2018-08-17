package core

import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.exceptions.ReentrantContinuationException
import core.exceptions.UndefinedIdentifierException
import core.procedures.AFn
import core.procedures.IFn
import core.procedures.predicates.Predicate
import core.scm.*
import core.scm.specialforms.New
import core.scm.specialforms.SpecialForm
import core.utils.Utils
import core.utils.cached
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong

class Evaluator(private val reflector: Reflector = Reflector(),
                private val macroexpander: Macroexpander = Macroexpander()) {

    companion object {
        /* Used by gensym to generate unique symbol ids */
        private val id = AtomicInteger(0)
        internal val nextID get() = id.incrementAndGet()

        /* Executor Service for Futures */
        private val threadCounter = AtomicLong(0)

        @Volatile
        var executor = Executors.newCachedThreadPool {
            Thread(it, "executor-thread-${threadCounter.getAndIncrement()}")
        }!!
    }

    /* Macroexpand S-expression, evaluate it and then return the result */
    fun macroexpandAndEvaluate(sexp: Any?, env: Environment) = eval(macroexpander.expand(sexp), env)

    /* Main eval */
    fun eval(sexp: Any?, env: Environment): Any? = evalIter(sexp, env).let {
        when (it) {
            /* Eagerly evaluate thunks */
            is Thunk<*> -> it.eval(it.context ?: env)
            is Ratio -> Utils.downcastNumber(it)
            else        -> it
        }
    }

    /**
     * One iteration of evaluation.
     * Returns the end result or a Thunk object.
     * If Thunk is returned, then eval() method (trampoline) continues evaluation.
     */
    private fun evalIter(sexp: Any?, env: Environment) = try {
        when (sexp) {
            is Symbol      -> sexp.eval(env)
            is List<*>     -> sexp.eval(env)
            is Map<*, *>   -> sexp.eval(env)
            is Vector      -> sexp.eval(env)
            is Set<*>      -> sexp.eval(env)
            is Sequence<*> -> sexp.eval(env)
            else           -> sexp
        }
    } catch (cc: CalledContinuation) {
        /* We have one-shot continuations only, not full continuations.
         * It means that we can't use the same continuation multiple times. */
        if (cc.continuation.invoked) throw ReentrantContinuationException()
        /* Continuation is still valid, rethrow it further (should be caught by callcc)  */
        throw cc
    }

    /* Evaluate Thunk */
    private tailrec fun Thunk<*>.eval(env: Environment): Any? {
        val result = evalIter(expr, context ?: env)
        return when (result) {
            is Thunk<*> -> result.eval(env)
            else -> result
        }
    }

    /* Evaluate Sequence */
    private fun Sequence<*>.eval(env: Environment) = map { eval(it, env) }.cached()

    /* Evaluate Symbol */
    private fun Symbol.eval(env: Environment) = env.resolve(this).let {
        when (it) {
            is SpecialForm -> throw IllegalSyntaxException(it.toString(), Writer.write(this))
            /* Check if it is a Java class. If not found, then assume it is a static field */
            Type.Undefined -> evalReflection(name)
            else -> it
        }
    }

    /* Evaluate list */
    private fun List<*>.eval(env: Environment): Any? {
        if (isEmpty()) throw IllegalSyntaxException("eval", Writer.write(this), "illegal empty application")
        /* Improper lists are not allowed */
        if (!Predicate.isProperList(this)) throw IllegalSyntaxException("eval", Writer.write(this))
        var op = this[0]
        /* Evaluate operator */
        when (op) {
            is List<*>, is Map<*, *>, is Vector -> op = eval(op, env)
            is Symbol -> {
                /* Lookup symbol */
                op = env.resolve(op)
                /* Inline Special Forms and Pure functions
                 * Doesn't help much, so commenting it out for now
                 * if (op is SpecialForm || (op is AFn<*, *> && op.isPure)) { this[0] = op } else */
                if (op === Type.Undefined) {
                    // TODO implement as a macro
                    /* Special case: constructor call If Symbol ends with . */
                    val symbolName = (this[0] as Symbol).name
                    if (symbolName.endsWith('.')) {
                        val form = listOf(New, Symbol.intern(symbolName.dropLast(1))) + drop(1)
                        return New.eval(form, env, this@Evaluator)
                    }
                    op = object : AFn<Any?, Any?>() {
                        override fun invoke(args: Array<out Any?>) = reflector.evalJavaMethod(symbolName, args)
                    }
                }
            }
        }
        /* Now decide how to evaluate everything else */
        return when (op) {
            /* Special Forms have special evaluation rules */
            is SpecialForm -> op.eval(this, env, this@Evaluator)
            /* Op is a valid invokable object (procedure)
             * Scheme has applicative order, so evaluate all arguments first
             * and then invoke operator (IFn) via helper method */
            is IFn<*, *> -> AFn.invokeN(op, drop(1).map { eval(it, env) }.toTypedArray())
            /* If operator is not invokable, then raise an error */
            else -> throw IllegalArgumentException("wrong type to apply: ${Writer.write(op)}")
        }
    }

    private fun evalReflection(name: String): Any? {
        if (name[0].isJavaIdentifierStart()) {
            val clazz = reflector.getClazzOrNull(name)
            if (clazz != null) {
                return clazz
            }
            if ('/' in name) {
                val (className, fieldName) = name.split('/').filterNot(String::isEmpty).apply {
                    if (size < 2) throw IllegalSyntaxException("reflector: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)")
                }
                return reflector.evalJavaStaticField(className, fieldName)
            }
        }
        throw UndefinedIdentifierException(name)
    }

    /* Evaluate hash map */
    private fun Map<*, *>.eval(env: Environment) = entries.associateTo(MutableHashmap()) { eval(it.key, env) to eval(it.value, env) }

    /* Evaluate vector */
    private fun Vector.eval(env: Environment) = apply { indices.forEach { array[it] = eval(array[it], env) } }

    /* Evaluate set */
    private fun Set<*>.eval(env: Environment) = mapTo(MutableSet(size)) { eval(it, env) }
}
