package core.procedures

import core.exceptions.ArityException
import core.exceptions.WrongTypeException
import core.scm.Type
import core.utils.Utils

/* Abstract superclass of all functions */
abstract class AFn : IFn<Any?, Any?>, Comparator<Any?> {

    protected var minArgs: Int = 0
    protected var maxArgs: Int = 0
    private   val mandatoryArgsTypes: Array<Class<*>>
    private   val restArgsType: Class<*>?
    private   val lastArgType: Class<*>?

    constructor() {
        minArgs = 0
        maxArgs = Integer.MAX_VALUE
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

    protected fun minArgs(): Int {
        return minArgs
    }

    /* Return true if function is pure (referentially transparent) */
    open val isPure
        get() = false

    open val name
        get() = javaClass.simpleName

    override fun compare(o1: Any?, o2: Any?): Int {
        val result = invokeN(o1, o2)
        if (result is Boolean) {
            if (Utils.toBoolean(result)) return -1
            if (Utils.toBoolean(invokeN(o2, o1))) return 1
            return 0
        }
        return (result as Number).toInt()
    }

    override fun run() {
        invoke()
    }

    @Throws(Exception::class)
    override fun call(): Any? {
        return invoke()
    }

    override fun apply(arg: Any?): Any? {
        return invoke(arg)
    }

    override operator fun invoke(): Any? {
        throw ArityException(name, minArgs, maxArgs, 1)
    }

    override operator fun invoke(arg: Any?): Any? {
        throw ArityException(name, minArgs, maxArgs, 1)
    }

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        throw ArityException(name, minArgs, maxArgs, 2)
    }

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): Any? {
        throw ArityException(name, minArgs, maxArgs, 3)
    }

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?, arg4: Any?): Any? {
        throw ArityException(name, minArgs, maxArgs, 4)
    }

    override operator fun invoke(vararg args: Any?): Any? {
        throw ArityException(name, minArgs, maxArgs, args.size)
    }

    override fun toString(): String {
        when (name.isEmpty()) {
            true  -> return "#<procedure>"
            false -> return "#<procedure:$name>"
        }
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
            if (mandatoryArgsTypes.isNotEmpty() && i < mandatoryArgsTypes.size) {
                if (!Type.checkType(arg, mandatoryArgsTypes[i])) {
                    throw WrongTypeException(name, mandatoryArgsTypes[i], arg)
                }
                continue
            }
            /* Last argument (optional special case) */
            if (i == argsSize - 1 && lastArgType != null) {
                if (!Type.checkType(arg, lastArgType)) {
                    throw WrongTypeException(name, lastArgType, arg)
                }
                continue
            }
            /* Rest args */
            if (restArgsType != null) {
                if (!Type.checkType(arg, restArgsType)) {
                    throw WrongTypeException(name, restArgsType, arg)
                }
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
        when (arity) {
            0    -> return invoke()
            1    -> return invoke(args[0])
            2    -> return invoke(args[0], args[1])
            3    -> return invoke(args[0], args[1], args[2])
            4    -> return invoke(args[0], args[1], args[2], args[3])
            else -> return invoke(*args)
        }
    }
}
