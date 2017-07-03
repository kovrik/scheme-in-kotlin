package core.procedures

import java.util.concurrent.Callable

interface IFn<in T, out R> : () -> R,
                             (T) -> R,
                             (T, T) -> R,
                             (T, T, T) -> R,
                             (T, T, T, T) -> R,
                             Callable<Any?>, Runnable {

    override operator fun invoke(): R
    override operator fun invoke(arg: T): R
    override operator fun invoke(arg1: T, arg2: T): R
    override operator fun invoke(arg1: T, arg2: T, arg3: T): R
    override operator fun invoke(arg1: T, arg2: T, arg3: T, arg4: T): R

    operator fun invoke(args: Array<out T>): R
}
