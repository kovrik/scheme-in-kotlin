package core.procedures;

import java.util.concurrent.Callable;
import java.util.function.Function;

public interface IFn<T, R> extends Function<T, R>, Callable, Runnable {

}
