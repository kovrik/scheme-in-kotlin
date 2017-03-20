package unittests;

import core.environment.DefaultEnvironment;
import core.environment.Environment;
import core.evaluator.Evaluator;
import core.reader.StringReader;
import core.scm.SCMSymbol;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;

public abstract class AbstractTest {

  private final StringReader reader = new StringReader();
  protected final Evaluator eval = new Evaluator();
  protected final DefaultEnvironment env = new DefaultEnvironment();
  {
    /* Eval lib procedures */
    for (String proc : env.getLibraryProcedures()) {
      for (Object p : reader.read(proc)) {
        eval.macroexpandAndEvaluate(p, env);
      }
    }
  }
  /* Helper method: evaluates first S-expression */
  protected Object eval(String sexp, Environment env) {
    return eval.macroexpandAndEvaluate(reader.readFirst(sexp), env);
  }

  protected SCMSymbol s(String str) {
    return SCMSymbol.of(str);
  }

  protected void assertAllEqual(Object expected, String[] forms, Environment env) {
    Arrays.stream(forms).forEach(form -> assertEquals(form, expected, eval(form, env)));
  }
}
