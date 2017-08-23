package core.scm

import core.environment.Environment
import core.procedures.AFn

/* Lambda */
class Procedure(override var name: String,
                /* Array of arguments the procedure expects */
                private val args: Array<Symbol?>,
                /* Body form of the procedure */
                private val body: Any,
                /* Lexical environment */
                private val localEnvironment: Environment,
                isVariadic: Boolean) : AFn<Any?, Any?>() {

    /* Is body a constant? If it is, then no need to evaluate it */
    private val isBodyConst: Boolean = body !is Symbol && body !is Collection<*> && body !is Map<*, *>

    init {
        if (isVariadic) {
            /* Do not count `rest` arg */
            this.minArgs = this.args.size - 1
        } else {
            this.minArgs = this.args.size
            this.maxArgs = this.args.size
        }
    }

    override operator fun invoke() = when {
        isBodyConst -> body
        else        -> Thunk(body, Environment(0, localEnvironment))
    }

    override operator fun invoke(arg: Any?) = when {
        isBodyConst -> body
        else -> Thunk(body, Environment(1, localEnvironment).apply { put(args[0], arg) })
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = when {
        isBodyConst -> body
        else -> Thunk(body, Environment(2, localEnvironment).apply {
            put(args[0], arg1)
            put(args[1], arg2)
        })
    }

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) = when {
        isBodyConst -> body
        else -> Thunk(body, Environment(3, localEnvironment).apply {
            put(args[0], arg1)
            put(args[1], arg2)
            put(args[2], arg3)
        })
    }

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?, arg4: Any?) = when {
        isBodyConst -> body
        else -> Thunk(body, Environment(4, localEnvironment).apply {
            put(args[0], arg1)
            put(args[1], arg2)
            put(args[2], arg3)
            put(args[3], arg4)
        })
    }

    override operator fun invoke(args: Array<out Any?>) = if (isBodyConst) body else Thunk(body, bindArgs(args))

    private fun bindArgs(values: Array<out Any?>) = Environment(values.size, localEnvironment).apply {
        /* Evaluate mandatory params and put values into new local environment */
        for (i in 0 until minArgs) {
            put(args[i], values[i])
        }
        if (minArgs != maxArgs) {
            /* Optional params: pass them as a list bound to the last param.
             * Everything AFTER mandatory params goes to that list. */
            put(args[minArgs], values.copyOfRange(minArgs, values.size).asList())
        }
    }

    /* Lambdas have no arg type information, hence nothing to check */
    override fun checkArgs(args: Array<out Any?>) = Unit
}
