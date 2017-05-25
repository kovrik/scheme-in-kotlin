package core.procedures

import java.util.concurrent.Callable
import java.util.function.Function

interface IFn<T, R> : Function<T, R>, Callable<Any?>, Runnable {

    operator fun invoke(): Any?

    operator fun invoke(arg: Any?): Any?

    operator fun invoke(arg1: Any?, arg2: Any?): Any?

    operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): Any?

    operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?, arg4: Any?): Any?

    operator fun invoke(vararg args: Any?): Any?
}
