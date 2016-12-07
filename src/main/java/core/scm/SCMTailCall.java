package core.scm;

import core.environment.IEnvironment;

// TODO Replace with Continuations
public class SCMTailCall {

  private final Object expr;
  private final IEnvironment context;

  public SCMTailCall(Object expr, IEnvironment context) {
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
