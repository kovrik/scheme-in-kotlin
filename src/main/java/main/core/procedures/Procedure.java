package main.core.procedures;

import main.core.evaluator.IEvaluator;
import main.environment.Environment;
import main.environment.IEnvironment;

import java.util.*;

// TODO Optimize
public class Procedure implements IFn {

  private List<Object> params;
  private Object body;
  private IEnvironment env;

  // FIXME Decouple
  private IEvaluator evaluator;

  public Procedure(List<Object> params, Object body, IEnvironment env) {
    this.params = (params == null) ? Collections.emptyList() : params;
    this.body = body;
    this.env = env;
    this.evaluator = null;
  }

  public Object call(List<Object> args) {
    if (args == null || args.size() != params.size()) {
      throw new IllegalArgumentException("Wrong number of arguments: expected " + params.size() +
                                         ", actual " + ((args == null) ? 0 : args.size()));
    }
    Map<Object, Object> values = new HashMap<Object, Object>(params.size());
    for (int i = 0; i < params.size(); i++) {
      values.put(params.get(i), args.get(i));
    }
    // FIXME
    Environment localEnvironment = new Environment(env);
    localEnvironment.putAll(values);
    return evaluator.eval(body, localEnvironment);
  }

  public Object invoke(Object... args) {
    return call(Arrays.asList(args));
  }

  public Object call() throws Exception {
    return invoke();
  }

  public void run() {
    invoke();
  }

  public Object getBody() {
    return body;
  }

  public List<Object> getParams() {
    return params;
  }
}
