package core.procedures

import core.exceptions.ArityException
import core.exceptions.WrongTypeException
import core.scm.Type
import core.utils.Utils

/* Abstract superclass of all functions */
abstract class AFn : IFn<Any?, Any?>, Comparator<Any?> {

    protected var minArgs: Int
    protected var maxArgs: Int
    private   val mandatoryArgsTypes: Array<Class<*>>
    private   val restArgsType: Class<*>?
    private   val lastArgType: Class<*>?

    constructor() {
        minArgs = 0
        maxArgs = Int.MAX_VALUE
        mandatoryArgsTypes = arrayOf<Class<*>>()
        restArgsType = null
        lastArgType = null
    }

    protected constructor(fnArgs: FnArgs) {
        this.minArgs = fnArgs.min
        this.maxArgs = fnArgs.max
        this.mandatoryArgsTypes = fnArgs.mandatory
        this.restArgsType = fnArgs.rest
        this.lastArgType = fnArgs.last
    }

    protected fun minArgs() = minArgs

    /* Return true if function is pure (referentially transparent) */
    open val isPure
        get() = false

    open val name: String
        get() = javaClass.simpleName

    override fun compare(o1: Any?, o2: Any?): Int {
        val result = invokeN(o1, o2)
        if (result is Boolean) {
            if (result) return -1
            if (Utils.toBoolean(invokeN(o2, o1))) return 1
            return 0
        }
        return (result as Number).toInt()
    }

    override fun run() {
        invoke()
    }

    @Throws(Exception::class)
    override fun call(): Any? = invoke()

    override operator fun invoke(): Any? = throw ArityException(name, minArgs, maxArgs, 1)
    override operator fun invoke(arg: Any?): Any? = throw ArityException(name, minArgs, maxArgs, 1)
    override operator fun invoke(arg1: Any?, arg2: Any?): Any? = throw ArityException(name, minArgs, maxArgs, 2)
    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): Any? = throw ArityException(name, minArgs, maxArgs, 3)
    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?, arg4: Any?): Any? = throw ArityException(name, minArgs, maxArgs, 4)
    override operator fun invoke(vararg args: Any?): Any? = throw ArityException(name, minArgs, maxArgs, args.size)

    override fun toString() = when (name.isEmpty()) {
        true  -> "#<procedure>"
        false -> "#<procedure:$name>"
    }

    /**
     * Checks the number of arguments and their types
     */
    private fun checkArgs(vararg args: Any?) {
        /* Check arg count */
        val argsSize = args.size
        if (argsSize < minArgs || argsSize > maxArgs) {
            throw ArityException(name, minArgs, maxArgs, argsSize)
        }
        for (i in 0..argsSize - 1) {
            val arg = args[i]
            /* Mandatory args */
            when {
                i < mandatoryArgsTypes.size              -> Type.assertType(name, arg, mandatoryArgsTypes[i])
                i == argsSize - 1 && lastArgType != null -> Type.assertType(name, arg, lastArgType)
                restArgsType != null                     -> Type.assertType(name, arg, restArgsType)
            }
        }
    }

    /**
     * Helper method that checks if FnArgs annotation is present,
     * if function is a fixed-arity function and if it is,
     * then calls invokeN() methods (where N is arity).
     * Calls variadic invoke() otherwise.
     */
    fun invokeN(vararg args: Any?): Any? {
        checkArgs(*args)
        /* if min == max, then function is not variadic, hence get arity */
        val arity = if (minArgs == maxArgs) minArgs else -1
        return when (arity) {
            0    -> invoke()
            1    -> invoke(args[0])
            2    -> invoke(args[0], args[1])
            3    -> invoke(args[0], args[1], args[2])
            4    -> invoke(args[0], args[1], args[2], args[3])
            else -> invoke(*args)
        }
    }
}
