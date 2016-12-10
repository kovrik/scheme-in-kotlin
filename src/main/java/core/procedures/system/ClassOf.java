package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.ISCMClass;
import core.scm.SCMClass;

import java.math.BigDecimal;
import java.util.List;

@FnArgs(args = {Object.class})
public class ClassOf extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "class-of";
  }

  @Override
  public Object invoke(Object... args) {
    Object object = args[0];
    if (object == null) {
      return SCMClass.NIL;
    }

    /* All custom SCM Classes should implement ISCMClass interface */
    if (object instanceof ISCMClass) {
      return ((ISCMClass)object).getSCMClass();
    }

    /* Must be a Java object */
    if (object instanceof BigDecimal) {
      /* Check if it is integral */
      if (((BigDecimal)object).remainder(BigDecimal.ONE).equals(BigDecimal.ZERO)) {
        return SCMClass.INTEGER;
      }
      return SCMClass.REAL;
    }
    /* Check Pair and Nil */
    if (object instanceof List) {
      if (((List) object).isEmpty()) {
        return SCMClass.NIL;
      }
      return SCMClass.PAIR;
    }
    return SCMClass.valueOf(object.getClass());
  }
}
