package main.functions;

import main.Main;
import main.ast.Node;
import main.environment.Environment;

import java.util.*;

// TODO Optimize
public class Procedure implements IFn {

  private List<Object> params;
  private Node body;
  private Environment env;

  public Procedure(List<Object> params, Node body, Environment env) {
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
    return Main.eval(body, new Environment(values, env));
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
