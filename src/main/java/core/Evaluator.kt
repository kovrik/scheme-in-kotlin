package core

import core.environment.DefaultEnvironment
import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.exceptions.ReentrantContinuationException
import core.exceptions.UndefinedIdentifierException
import core.procedures.AFn
import core.procedures.IFn
import core.scm.*
import core.scm.specialforms.SpecialForm
import core.utils.Utils
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong

class Evaluator(val env: Environment = DefaultEnvironment(),
                private val reflector: Reflector = Reflector(),
                private val macroexpander: Macroexpander = Macroexpander(),
                private val name: String = "eval") {

    companion object {
        /* Used by gensym to generate unique symbol ids */
        private val id = AtomicInteger(0)
        internal val nextID get() = id.incrementAndGet()

        private val threadCounter = AtomicLong(0)

        /* Executor Service for Futures */
        @Volatile
        var executor: ExecutorService = Executors.newCachedThreadPool {
            Thread(it, "executor-thread-${threadCounter.getAndIncrement()}")
        }
    }

    /* Macroexpand S-expression, evaluate it and then return the result */
    fun macroexpandAndEvaluate(sexp: Any?) = eval(macroexpander.expand(sexp))

    /* Main eval */
    fun eval(sexp: Any?): Any? = when (val result = evalIter(sexp)) {
        /* Eagerly evaluate thunks */
        is Thunk<*>    -> result.eval()
        is Ratio       -> Utils.downcastNumber(result)
        is SpecialForm -> throw IllegalSyntaxException(result.toString(), Writer.write(sexp))
        else           -> result
    }

    /**
     * One iteration of evaluation.
     * Returns the end result or a Thunk object.
     * If Thunk is returned, then eval() method (trampoline) continues evaluation.
     */
    private fun evalIter(sexp: Any?) = try {
            when (sexp) {
                is Symbol      -> sexp.eval()
                is List<*>     -> sexp.eval()
                is Map<*, *>   -> sexp.eval()
                is Vector      -> sexp.eval()
                is Set<*>      -> sexp.eval()
                is Sequence<*> -> sexp.eval()
                is Pair<*, *>  -> throw IllegalSyntaxException(name, Writer.write(sexp), "wrong type to apply")
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
    private tailrec fun Thunk<*>.eval(): Any? = when (val result = Evaluator(context ?: env).evalIter(expr)) {
        is Thunk<*> -> result.eval()
        else -> result
    }

    /* Evaluate Thunk */
//    override tailrec fun visitThunk(thunk: Thunk<*>): Any? = when (val result = Evaluator(thunk.context ?: env).evalIter(thunk.expr)) {
//        is Thunk<*> -> visitThunk(result)
//        else -> result
//    }

    /* Evaluate Sequence */
    private fun Sequence<*>.eval() = map { eval(it) }.cached()

    /* Evaluate Symbol */
    private fun Symbol.eval() = when (val result = env.resolve(this)) {
        /* Assume it is a Java static field */
        Unit -> evalJavaStaticField(name)
        else -> result
    }

    /* Evaluate list */
    private fun List<*>.eval(): Any? {
        /* Evaluate operator */
        val op = firstOrNull().let {
            when (it) {
                null -> throw IllegalSyntaxException(name, Writer.write(this), "illegal empty application")
                is List<*>, is Map<*, *>, is Vector -> eval(it)
                is Symbol -> env.resolve(it)
                else -> it
            }
        }
        /* Now decide how to evaluate everything else */
        return when (op) {
            /* Special Forms have special evaluation rules */
            is SpecialForm -> op.eval(this, this@Evaluator)
            /* Op is a valid invokable object (procedure)
             * Scheme has applicative order, so evaluate all arguments first
             * and then invoke operator (IFn) via helper method */
            is IFn<*, *> -> AFn.invokeN(op, drop(1).map { eval(it) }.toTypedArray())
            // TODO implement as a macro
            is Unit -> reflector.evalJavaMethod((this[0] as Symbol).name, drop(1).map { eval(it) }.toTypedArray())
            /* If operator is not invokable, then raise an error */
            else -> throw IllegalSyntaxException(name, Writer.write(this), "wrong type to apply")
        }
    }

    private fun evalJavaStaticField(name: String) = reflector.getClazzOrNull(name) ?: when {
        '/' in name -> {
            val (className, fieldName) = name.split('/').filterNot(String::isEmpty).apply {
                if (size < 2) throw IllegalSyntaxException("${reflector}: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)")
            }
            reflector.evalJavaStaticField(className, fieldName)
        }
        else -> throw UndefinedIdentifierException(name)
    }

    /* Evaluate hash map */
    private fun Map<*, *>.eval() = entries.associateTo(MutableHashmap()) { eval(it.key) to eval(it.value) }

    /* Evaluate vector */
    private fun Vector.eval() = apply { indices.forEach { array[it] = eval(array[it]) } }

    /* Evaluate set */
    private fun Set<*>.eval() = mapTo(MutableSet(size)) { eval(it) }
}
