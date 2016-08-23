package core.scm.specialforms.tco;

import core.environment.IEnvironment;

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
