package core.procedures.delayed;

import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class SCMPromise extends SCMProcedure {

  private Object result;

  public SCMPromise(List<SCMSymbol> params, Object body) {
    super("promise", params, body);
  }

  public Object getResult() {
    return result;
  }

  public void setResult(Object result) {
    this.result = result;
  }

  @Override
  public String getName() {
    return "#<promise " + hashCode() + ">";
  }
}
