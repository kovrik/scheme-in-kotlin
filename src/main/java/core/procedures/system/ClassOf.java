package core.procedures.system;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.procedures.IFn;
import core.procedures.delayed.SCMPromise;
import core.scm.SCMProcedure;
import core.scm.specialforms.SCMSpecialForm;

public class ClassOf extends AFn {

  // TODO Create SCMClass class?

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, "class-of");
    }

    Object object = args[0];
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
    if (object instanceof IFn) {
      return IFn.class.getName();
    }
    return object.getClass().getName();
  }
}
