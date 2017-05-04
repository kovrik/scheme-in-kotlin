package core.procedures;

import core.scm.ITyped;
import core.scm.Type;

import java.util.concurrent.Callable;
import java.util.function.Function;

public interface IFn<T, R> extends ITyped, Function<T, R>, Callable, Runnable {

  @Override
  default Type getType() {
    return Type.PROCEDURE;
  }
}
