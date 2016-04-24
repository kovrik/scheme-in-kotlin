package main.core.procedures;

import java.util.Collections;
import java.util.List;

public class Procedure {

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
}
