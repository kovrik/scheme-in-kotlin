package core.procedures;

import java.util.concurrent.Callable;
import java.util.function.Function;

public interface IFn<T, R> extends Function<T, R>, Callable, Runnable {

  Object apply0();

  Object apply1(Object arg);

  Object apply2(Object arg1, Object arg2);

  Object apply3(Object arg1, Object arg2, Object arg3);

  Object apply4(Object arg1, Object arg2, Object arg3, Object arg4);

  Object apply(Object... args);
}
