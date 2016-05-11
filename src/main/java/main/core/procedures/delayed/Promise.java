package main.core.procedures.delayed;

import main.core.procedures.Procedure;

import java.util.List;

public class Promise extends Procedure {

  private Object result;

  public Promise(List<Object> params, Object body) {
    super(params, body);
  }

  public Object getResult() {
    return result;
  }

  public void setResult(Object result) {
    this.result = result;
  }
}
