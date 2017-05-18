package core.procedures

import core.exceptions.ArityException
import core.exceptions.WrongTypeException
import core.scm.Type

/* Abstract superclass of all functions */
abstract class AFn : IFn<Array<Any>, Any> {

    protected var minArgs: Int = 0
    protected var maxArgs: Int = 0
    private val mandatoryArgsTypes: Array<Class<*>>
    private val restArgsType: Class<*>?
    private val lastArgType: Class<*>?

    constructor() {
        minArgs = 0
        maxArgs = Integer.MAX_VALUE
        mandatoryArgsTypes = arrayOf<Class<*>>()
        restArgsType = null
        lastArgType = null
    }

    protected constructor(fnArgs: FnArgs) {
        this.minArgs = fnArgs.min()
        this.maxArgs = fnArgs.max()
        this.mandatoryArgsTypes = fnArgs.mandatory()
        this.restArgsType = fnArgs.rest()
        this.lastArgType = fnArgs.last()
    }

    protected fun minArgs(): Int {
        return minArgs
    }

    /* Return true if function is pure (referentially transparent) */
    open val isPure: Boolean
        get() = false

    override fun run() {
        apply0()
    }

    @Throws(Exception::class)
    override fun call(): Any {
        return apply0()
    }

    override fun apply0(): Any {
        throw ArityException(name, minArgs, maxArgs, 1)
    }

    override fun apply1(arg: Any): Any {
        throw ArityException(name, minArgs, maxArgs, 1)
    }

    override fun apply2(arg1: Any, arg2: Any): Any {
        throw ArityException(name, minArgs, maxArgs, 2)
    }

    override fun apply3(arg1: Any, arg2: Any, arg3: Any): Any {
        throw ArityException(name, minArgs, maxArgs, 3)
    }

    override fun apply4(arg1: Any, arg2: Any, arg3: Any, arg4: Any): Any {
        throw ArityException(name, minArgs, maxArgs, 4)
    }

//    override fun apply(vararg args: Any): Any {
//        throw ArityException(name, minArgs, maxArgs, args.size)
//    }

    override fun apply(args: Array<Any>): Any {
        throw ArityException(name, minArgs, maxArgs, args.size)
    }

    open val name: String
        get() = javaClass.simpleName

    override fun toString(): String {
        val name = name
        when {
            name.isEmpty() -> return "#<procedure>"
            else -> return "#<procedure:$name>"
        }
    }

    /**
     * Checks the number of arguments and their types
     */
    private fun checkArgs(args: Array<Any>) {
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
     * then calls applyN() methods (where N is arity).
     * Calls variadic apply() otherwise.
     */
    fun applyN(args: Array<Any>): Any {
        /* Check args */
        checkArgs(args)
        /* if min == max, then function is not variadic, hence get arity */
        val arity = if (minArgs == maxArgs) minArgs else -1
        when (arity) {
            0 -> return apply0()
            1 -> return apply1(args[0])
            2 -> return apply2(args[0], args[1])
            3 -> return apply3(args[0], args[1], args[2])
            4 -> return apply4(args[0], args[1], args[2], args[3])
            else -> return apply(args)
        }
    }
}
