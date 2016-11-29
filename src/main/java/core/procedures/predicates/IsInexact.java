package core.procedures.predicates;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;
import core.scm.SCMBoolean;

import java.math.BigDecimal;
import java.math.BigInteger;

public class IsInexact extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "inexact?";
  }

  @Override
  public SCMBoolean invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    if (!(args[0] instanceof Number)) {
      throw new WrongTypeException("Number", args[0]);
    }
    return SCMBoolean.toSCMBoolean(isInexact(args[0]));
  }

  public static boolean isInexact(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    if (o instanceof Long || o instanceof SCMBigRational || o instanceof Integer || o instanceof BigInteger) {
      return false;
    }
    if (o instanceof BigDecimal) {
      return ((BigDecimal)o).scale() != 0;
    }
    return true;
  }
}
