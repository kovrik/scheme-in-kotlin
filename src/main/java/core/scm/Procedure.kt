package core.scm

import core.environment.Environment
import core.procedures.AFn

/* Lambda */
class Procedure(override var name: String,
                /* Array of arguments the procedure expects */
                private val args: Array<Symbol?>,
                /* Body form of the procedure */
                private val body: Any?,
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

    private fun bindArgs(values: Array<out Any?>): Environment {
        /* Evaluate mandatory params and put values into new local environment */
        val env = Environment(values.size, this.localEnvironment)
        for (i in 0..minArgs - 1) {
            env.put(args[i], values[i])
        }
        /* If it is a variadic function, then evaluate rest param */
        if (minArgs != maxArgs) {
            /* Optional params: pass them as a list bound to the last param.
             * Everything AFTER mandatory params goes to that list. */
            env.put(args[minArgs], values.copyOfRange(minArgs, values.size).toList())
        }
        return env
    }

    override operator fun invoke() = when {
        isBodyConst -> body
        else        -> Thunk(body, Environment(0, this.localEnvironment))
    }

    override operator fun invoke(arg: Any?): Any? {
        if (isBodyConst) {
            return body
        }
        val environment = Environment(1, this.localEnvironment)
        environment.put(args[0], arg)
        return Thunk(body, environment)
    }

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        if (isBodyConst) {
            return body
        }
        val environment = Environment(2, this.localEnvironment)
        environment.put(args[0], arg1)
        environment.put(args[1], arg2)
        return Thunk(body, environment)
    }

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): Any? {
        if (isBodyConst) {
            return body
        }
        val environment = Environment(3, this.localEnvironment)
        environment.put(args[0], arg1)
        environment.put(args[1], arg2)
        environment.put(args[2], arg3)
        return Thunk(body, environment)
    }

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?, arg4: Any?): Any? {
        if (isBodyConst) {
            return body
        }
        val environment = Environment(4, this.localEnvironment)
        environment.put(args[0], arg1)
        environment.put(args[1], arg2)
        environment.put(args[2], arg3)
        environment.put(args[3], arg4)
        return Thunk(body, environment)
    }

    override operator fun invoke(args: Array<out Any?>) = if (isBodyConst) body else Thunk(body, bindArgs(args))
}
