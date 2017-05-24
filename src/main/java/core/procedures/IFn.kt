package core.procedures

import java.util.concurrent.Callable
import java.util.function.Function

interface IFn<T, R> : Function<T, R>, Callable<Any?>, Runnable {

    fun apply0(): Any?

    fun apply1(arg: Any?): Any?

    fun apply2(arg1: Any?, arg2: Any?): Any?

    fun apply3(arg1: Any?, arg2: Any?, arg3: Any?): Any?

    fun apply4(arg1: Any?, arg2: Any?, arg3: Any?, arg4: Any?): Any?

    fun apply(vararg args: Any?): Any?
}
