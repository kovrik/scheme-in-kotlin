package core.scm;

import core.environment.Environment;
import core.evaluator.Evaluator;
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

  private final Object body;
  private Object result;
  private State state;

  public SCMPromise(Object body) {
    this.body = body;
    this.state = PENDING;
  }

  /**
   * Evaluate forced Promise
   */
  public Object force(Environment env, Evaluator evaluator) {
    try {
      /* Evaluate the body */
      Object result = evaluator.eval(getBody(), env);
      /* Mark Promise as FULFILLED */
      setState(SCMPromise.State.FULFILLED);
      /* Memoize the result */
      setResult(result);
      return result;
    } catch (Exception e) {
      /* Mark Promise as REJECTED */
      setState(SCMPromise.State.REJECTED);
      /* Memoize the result */
      setResult(e);
      throw e;
    }
  }

  public Object getBody() {
    return body;
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
    StringBuilder sb = new StringBuilder("#<promise");
    if (state == REJECTED) {
      sb.append("!error!").append(Writer.write(result));
    }
    if (state == FULFILLED) {
      sb.append("!").append(Writer.write(result));
    }
    return sb.append(">").toString();
  }
}
