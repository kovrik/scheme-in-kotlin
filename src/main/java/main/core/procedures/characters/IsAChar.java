package main.core.procedures.characters;

import main.core.procedures.IFn;

import java.util.concurrent.ExecutionException;

public class IsAChar implements IFn {

  public Object invoke(Object... args) throws ExecutionException, InterruptedException {
    if (args.length < 1) {
      throw new IllegalArgumentException("Wrong number of arguments to `char?`");
    }
    for (Object arg : args) {
      if (!(arg instanceof Character)) {
        return Boolean.FALSE;
      }
    }
    return Boolean.TRUE;
  }

  public Object call() throws Exception {
    return Boolean.FALSE;
  }

  public void run() {
  }
}
