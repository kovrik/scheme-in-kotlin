package main.core.procedures;

import main.core.ast.SCMList;
import main.core.ast.SCMSymbol;

import java.util.Collections;

// FIXME
public class Force extends Procedure {

  private static final SCMList<Object> FORCE = new SCMList<Object>(Collections.singletonList((Object)new SCMSymbol("promise")));

  public Force() {
    super(FORCE, FORCE);
  }

  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new IllegalArgumentException("Wrong number of arguments to `force`");
    }
    if (!(args[0] instanceof Promise)) {
      throw new IllegalArgumentException("Wrong type argument to `force`");
    }
    return this;
  }

  public Object call() throws Exception {
    return invoke();
  }

  public void run() {
  }
}
