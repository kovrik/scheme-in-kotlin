package s7.tests;

import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.reader.IReader;
import core.reader.Reader;

public abstract class AbstractS7Test {

  private final IReader reader = new Reader();
  protected final IEvaluator eval = new Evaluator();
  protected final DefaultEnvironment env = new DefaultEnvironment();
  {
    /* Eval lib procedures */
    for (String proc : env.getLibraryProcedures()) {
      eval(proc, env);
    }
  }
  /* Helper method */
  protected Object eval(String sexp, IEnvironment env) {
    return eval.eval(reader.read(sexp), env);
  }
}
