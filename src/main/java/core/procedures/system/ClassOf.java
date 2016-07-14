package core.procedures.system;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.procedures.delayed.SCMPromise;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.specialforms.SCMSpecialForm;

import java.util.List;

public class ClassOf extends SCMProcedure {

  private static final SCMSymbol obj = new SCMSymbol("obj");
  private static final List<SCMSymbol> params = SCMCons.list(obj);

  public ClassOf() {
    super("class-of", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object object = env.get(obj);
    /* All special forms have the same SCMSpecialForm class */
    if (SCMSpecialForm.class.equals(object.getClass().getSuperclass())) {
      return SCMSpecialForm.class.getName();
    }
    /* Promise is a procedure, but want it to have a separate class */
    if (object instanceof SCMPromise) {
      return SCMPromise.class.getName();
    }
    /* Do not return exact class of procedure */
    if (object instanceof SCMProcedure) {
      return SCMProcedure.class.getName();
    }
    return object.getClass().getName();
  }
}
