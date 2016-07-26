package core.scm;

import static core.scm.SCMPromise.State.PENDING;

public class SCMPromise extends SCMProcedure implements ISCMClass {

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
  public SCMClass getSCMClass() {
    return SCMClass.PROMISE;
  }

  @Override
  public String toString() {
    return "#<promise " + hashCode() + ": " + state + ">";
  }
}
