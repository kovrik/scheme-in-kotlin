package core.scm;

import core.writer.Writer;

import static core.scm.SCMPromise.State.FULFILLED;
import static core.scm.SCMPromise.State.PENDING;
import static core.scm.SCMPromise.State.REJECTED;

public class SCMPromise implements ISCMClass {

  public enum State {
    PENDING,
    FORCED,
    FULFILLED,
    REJECTED
  }

  private final Object expr;
  private Object value;
  private State state;

  public SCMPromise(Object expr) {
    this.expr = expr;
    this.state = PENDING;
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

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.PROMISE;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("#<promise");
    if (state == REJECTED) {
      sb.append("!error!").append(Writer.write(value));
    }
    if (state == FULFILLED) {
      sb.append("!").append(Writer.write(value));
    }
    return sb.append(">").toString();
  }
}
