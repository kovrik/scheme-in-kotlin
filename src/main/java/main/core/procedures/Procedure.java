package main.core.procedures;

import main.core.IEvaluator;
import main.environment.Environment;

import java.util.*;

// TODO Optimize
public class Procedure implements IFn {

  private List<Object> params;
  private Object body;
  private Environment env;

  private IEvaluator evaluator;

  public Procedure(List<Object> params, Object body, Environment env, IEvaluator evaluator) {
    this.params = (params == null) ? Collections.emptyList() : params;
    this.body = body;
    this.env = env;
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
    return evaluator.eval(body, new Environment(values, env));
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
}
