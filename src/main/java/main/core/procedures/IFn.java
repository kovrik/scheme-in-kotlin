package main.core.procedures;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

public interface IFn extends Runnable, Callable {

  Object invoke(Object... args) throws ExecutionException, InterruptedException;
}
