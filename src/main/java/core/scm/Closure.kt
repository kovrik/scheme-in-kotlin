package core.scm

import core.environment.Environment
import core.procedures.AFn
import core.procedures.Arity
import core.procedures.Arity.AtLeast

class Closure(/* Array of arguments the procedure expects */
              private val args: List<Symbol?>,
              /* Body form of the procedure */
              private val body: Any?,
              /* Lexical environment */
              private val localEnvironment: Environment,
              override var arity: Arity = AtLeast(0)) : AFn<Any?, Any?>(arity = arity) {

    override operator fun invoke() = Thunk(body, Environment(0, localEnvironment))

    override operator fun invoke(arg: Any?) = Thunk(body, Environment(1, localEnvironment).apply { put(args[0], arg) })

    override operator fun invoke(arg1: Any?, arg2: Any?) = Thunk(body, Environment(2, localEnvironment).apply {
        put(args[0], arg1)
        put(args[1], arg2)
    })

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) = Thunk(body, Environment(3, localEnvironment).apply {
        put(args[0], arg1)
        put(args[1], arg2)
        put(args[2], arg3)
    })

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?, arg4: Any?) = Thunk(body, Environment(4, localEnvironment).apply {
        put(args[0], arg1)
        put(args[1], arg2)
        put(args[2], arg3)
        put(args[3], arg4)
    })

    override operator fun invoke(args: Array<out Any?>) = Thunk(body, bindArgs(args))

    private fun bindArgs(values: Array<out Any?>) = Environment(values.size, localEnvironment).apply {
        /* Evaluate mandatory params and put values into new local environment */
        for (i in 0 until args.size - 1) {
            put(args[i], values[i])
        }
        when (arity is Arity.AtLeast) {
            /* Optional params: pass them as a list bound to the last param.
             * Everything AFTER mandatory params goes to that list. */
            true  -> put(args[args.size - 1], values.copyOfRange(args.size - 1, values.size).asList())
            false -> put(args[args.size - 1], values[args.size - 1])
        }
    }

    /* Lambdas have no arg type information, hence nothing to check */
    override fun checkArgs(args: Array<out Any?>) = Unit
}
