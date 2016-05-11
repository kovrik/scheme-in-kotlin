package main.core.procedures;

import java.util.Collections;
import java.util.List;

// TODO Implement IFn?
public class Procedure implements IFn {

  private List<Object> params;
  private Object body;

  public Procedure(List<Object> params, Object body) {
    this.params = (params == null) ? Collections.emptyList() : params;
    this.body = body;
  }

  public Object getBody() {
    return body;
  }

  public List<Object> getParams() {
    return params;
  }

  // FIXME ====================================
  public Object invoke(Object... args) {
    return null;
  }

  public Object call() throws Exception {
    return null;
  }

  public void run() {

  }
  // FIXME ====================================
}
