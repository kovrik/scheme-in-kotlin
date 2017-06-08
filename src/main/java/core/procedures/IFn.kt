package core.procedures

import java.util.concurrent.Callable

interface IFn<in T, out R> : (T) -> R, Callable<Any?>, Runnable {

    operator fun invoke(): R

    override operator fun invoke(arg: T): R

    operator fun invoke(arg1: T, arg2: T): R

    operator fun invoke(arg1: T, arg2: T, arg3: T): R

    operator fun invoke(arg1: T, arg2: T, arg3: T, arg4: T): R

    operator fun invoke(vararg args: T): R
}
