package core.procedures.delayed;

import core.scm.SCMProcedure;

import static core.procedures.delayed.SCMPromise.State.PENDING;

public class SCMPromise extends SCMProcedure {

  public enum State {
    PENDING,
    FORCED, // TODO Get rid of this status
    FULFILLED,
    REJECTED
  }

  private Object result;
  private State state;

  public SCMPromise(Object body) {
    super("promise", null, body);
    this.state = PENDING;
  }

  public Object getResult() {
    return result;
  }

  public void setResult(Object result) {
    this.result = result;
  }

  public State getState() {
    return state;
  }

  public void setState(State state) {
    this.state = state;
  }

  @Override
  public String getName() {
    return "#<promise " + hashCode() + ": " + state + ">";
  }
}
