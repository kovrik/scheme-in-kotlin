package core.procedures

import core.exceptions.ArityException
import core.scm.Type
import core.utils.Utils

/* Abstract superclass of all functions */
abstract class AFn<T, out R>(var minArgs: Int = 0,
                             var maxArgs: Int = Int.MAX_VALUE,
                             val mandatoryArgsTypes: Array<Class<*>> = emptyArray(),
                             val restArgsType: Class<*>? = null,
                             val lastArgType: Class<*>? = null,
                             open val name: String = "",
                             /* Return true if function is pure (referentially transparent) */
                             open val isPure: Boolean = false) : IFn<T, R>, Comparator<T> {

    override fun arity() = if (minArgs == maxArgs) minArgs else -1

    override fun compare(o1: T, o2: T): Int = invoke(o1, o2).let {
        if (it is Boolean) {
            if (it) return -1
            if (Utils.toBoolean(invoke(o2, o1))) return 1
            return 0
        }
        return (it as Number).toInt()
    }

    override fun run() {
        invoke()
    }

    @Throws(Exception::class)
    override fun call(): Any? = invoke()

    override operator fun invoke(): R = throw ArityException(name, minArgs, maxArgs, 1)
    override operator fun invoke(arg: T): R = throw ArityException(name, minArgs, maxArgs, 1)
    override operator fun invoke(arg1: T, arg2: T): R = throw ArityException(name, minArgs, maxArgs, 2)
    override operator fun invoke(arg1: T, arg2: T, arg3: T): R = throw ArityException(name, minArgs, maxArgs, 3)
    override operator fun invoke(arg1: T, arg2: T, arg3: T, arg4: T): R = throw ArityException(name, minArgs, maxArgs, 4)

    override operator fun invoke(args: Array<out T>): R = throw ArityException(name, minArgs, maxArgs, args.size)

    override fun toString() = when (name.isEmpty()) {
        true  -> "#<procedure>"
        false -> "#<procedure:$name>"
    }

    override fun checkArity(args: Array<out T>) {
        if (args.size < minArgs || args.size > maxArgs) throw ArityException(name, minArgs, maxArgs, args.size)
    }

    /* Check args types */
    override fun checkArgs(args: Array<out T>) {
        for (i in 0..mandatoryArgsTypes.size - 1) { Type.assertType(name, args[i], mandatoryArgsTypes[i]) }
        for (i in mandatoryArgsTypes.size..args.size - 1) {
            when {
                lastArgType  != null && i == args.size - 1 -> Type.assertType(name, args[i], lastArgType)
                restArgsType != null                       -> Type.assertType(name, args[i], restArgsType)
            }
        }
    }

    /**
     * Helper method that checks args count and their types,
     * if function is a fixed-arity function and if it is,
     * then calls invoke<N>() methods (where N is arity).
     * Calls variadic invoke() otherwise.
     */
    companion object {
        fun <T, R> invokeN(fn: IFn<*, *>, args: Array<out T>): R = with(fn as IFn<T, R>) {
            checkArity(args)
            checkArgs(args)
            return when (arity()) {
                0    -> invoke()
                1    -> invoke(args[0])
                2    -> invoke(args[0], args[1])
                3    -> invoke(args[0], args[1], args[2])
                4    -> invoke(args[0], args[1], args[2], args[3])
                else -> invoke(args)
            }
        }
    }
}
