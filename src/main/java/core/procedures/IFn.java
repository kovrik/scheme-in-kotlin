package core.procedures;

import core.scm.ISCMClass;
import core.scm.SCMClass;

import java.util.concurrent.Callable;
import java.util.function.Function;

public interface IFn<T, R> extends ISCMClass, Function<T, R>, Callable, Runnable {

  @Override
  default SCMClass getSCMClass() {
    return SCMClass.PROCEDURE;
  }
}
