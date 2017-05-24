package core.scm

import core.environment.Environment
import core.procedures.AFn
import java.util.*

/* Lambda */
class Procedure(override var name: String,
                /* Array of arguments the procedure expects */
                private val args: Array<Symbol>,
                /* Body form of the procedure */
                private val body: Any?,
                localEnvironment: Environment,
                isVariadic: Boolean) : AFn() {

    companion object {
        private fun isConst(obj: Any?): Boolean {
            return !(obj is Symbol || obj is Collection<*> || obj is Map<*, *>)
        }
    }

    /* Is body a constant? If it is, then no need to evaluate it */
    private val isBodyConst: Boolean

    /* Lexical environment */
    private var localEnvironment: Environment? = null

    constructor(name: String,
                args: List<Symbol>,
                body: Any?,
                localEnvironment: Environment,
                isVariadic: Boolean) : this(name, args.toTypedArray(), body, localEnvironment, isVariadic)

    init {
        this.isBodyConst = isConst(body)
        this.localEnvironment = localEnvironment
        if (isVariadic) {
            /* Do not count rest arg */
            this.minArgs = this.args.size - 1
        } else {
            this.minArgs = this.args.size
            this.maxArgs = this.args.size
        }
    }

    private fun bindArgs(vararg values: Any?): Environment {
        /* Evaluate mandatory params and put values into new local environment */
        val env = Environment(values.size, this.localEnvironment)
        val args = args
        for (i in 0..minArgs - 1) {
            env.put(args[i], values[i])
        }
        /* If it is a variadic function, then evaluate rest param */
        if (minArgs != maxArgs) {
            /* Optional params: pass them as a list bound to the last param.
             * Everything AFTER mandatory params goes to that list. */
            env.put(args[minArgs()], Arrays.asList(*Arrays.copyOfRange(values, minArgs(), values.size)))
        }
        return env
    }

    override fun apply0(): Any? {
        if (isBodyConst) {
            return body
        }
        return Thunk(body, Environment(0, this.localEnvironment))
    }

    override fun apply1(arg: Any?): Any? {
        if (isBodyConst) {
            return body
        }
        val environment = Environment(1, this.localEnvironment)
        environment.put(args[0], arg)
        return Thunk(body, environment)
    }

    override fun apply2(arg1: Any?, arg2: Any?): Any? {
        if (isBodyConst) {
            return body
        }
        val environment = Environment(2, this.localEnvironment)
        environment.put(args[0], arg1)
        environment.put(args[1], arg2)
        return Thunk(body, environment)
    }

    override fun apply3(arg1: Any?, arg2: Any?, arg3: Any?): Any? {
        if (isBodyConst) {
            return body
        }
        val environment = Environment(3, this.localEnvironment)
        environment.put(args[0], arg1)
        environment.put(args[1], arg2)
        environment.put(args[2], arg3)
        return Thunk(body, environment)
    }

    override fun apply4(arg1: Any?, arg2: Any?, arg3: Any?, arg4: Any?): Any? {
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

    override fun apply(args: Array<Any?>): Any? {
        return if (isBodyConst) body else Thunk(body, bindArgs(*args))
    }
}
