package core.scm.specialforms;

import core.environment.IEnvironment;

// TODO Replace with Continuations
public class TailCall {

  private final Object expr;
  private final IEnvironment context;

  public TailCall(Object expr, IEnvironment context) {
    this.expr = expr;
    this.context = context;
  }

  public Object getExpr() {
    return expr;
  }

  public IEnvironment getContext() {
    return context;
  }
}
