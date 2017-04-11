package core.scm;

import core.writer.Writer;

import static core.scm.SCMPromise.State.*;

public class SCMPromise implements ISCMClass {

  public enum State {
    PENDING,
    FORCED,
    FULFILLED,
    REJECTED
  }

  private final Object expr;
  protected volatile Object value;
  private volatile State state = PENDING;

  public SCMPromise(Object expr) {
    this.expr = expr;
  }

  public Object getExpr() {
    return expr;
  }

  public Object getValue() {
    return value;
  }

  public void setValue(Object value) {
    this.value = value;
  }

  public State getState() {
    return state;
  }

  public void setState(State state) {
    this.state = state;
  }

  protected String getName() {
    return "promise";
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.PROMISE;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("#<").append(getName());
    switch (state) {
      case REJECTED:
        sb.append("!error!").append(Writer.write(value));
        break;
      case FULFILLED:
        sb.append("!").append(Writer.write(value));
        break;
      case PENDING:
      case FORCED:
        sb.append(":pending");
        break;
    }
    return sb.append(">").toString();
  }
}
