package s7.tests;

import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.reader.IReader;
import core.reader.Reader;

import java.util.Map;

public abstract class AbstractS7Test {

  protected final IReader reader = new Reader();
  protected final IEvaluator eval = new Evaluator();
  protected final DefaultEnvironment env = new DefaultEnvironment();
  {
    /* Eval lib procedures */
    for (Map.Entry<String, String> entry : env.getProcs().entrySet()) {
      env.put(entry.getKey(), eval(entry.getValue(), env));
    }
  }
  /* Helper method */
  protected Object eval(String sexp, IEnvironment env) {
    return eval.eval(reader.read(sexp), env);
  }
}
