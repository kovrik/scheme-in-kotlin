package main.functions;

import java.util.concurrent.Callable;

public interface IFn extends Runnable, Callable {

  Object invoke(Object... args);
}
