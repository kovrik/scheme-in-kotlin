package core.procedures.generic;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.utils.NumberUtils;

import java.util.List;

public class Range extends AFn {

  public Range() {
    super(new FnArgsBuilder().minArgs(0).maxArgs(3).restArgsType(SCMClass.Real.class));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "range";
  }

  @Override
  public List<Number> apply(Object... args) {
    if (args.length == 0) {
      return SCMCons.EMPTY;
    }
    // TODO Big numbers
    // TODO Fractions
    SCMCons<Number> result = SCMCons.list();
    boolean exact = NumberUtils.isExactInteger(args[0]);
    if (args.length == 3) {
      exact = exact && NumberUtils.isExactInteger(args[2]);
    }
    if (exact) {
      if (args.length == 1) {
        long end = ((Number) args[0]).longValue();
        for (long n = 0; n < end; n++) {
          result.add(n);
        }
      } else if (args.length == 2) {
        long start = ((Number) args[0]).longValue();
        long end   = ((Number) args[1]).longValue();
        for (long n = start; n < end; n++) {
          result.add(n);
        }
      } else if (args.length == 3) {
        long start = ((Number) args[0]).longValue();
        long end   = ((Number) args[1]).longValue();
        long step  = ((Number) args[2]).longValue();
        for (long n = start; n < end; n += step) {
          result.add(n);
        }
      }
    } else {
      if (args.length == 1) {
        double end = ((Number) args[0]).doubleValue();
        for (double n = 0; n < end; n++) {
          result.add(n);
        }
      } else if (args.length == 2) {
        double start = ((Number) args[0]).doubleValue();
        double end   = ((Number) args[1]).doubleValue();
        for (double n = start; n < end; n++) {
          result.add(n);
        }
      } else if (args.length == 3) {
        double start = ((Number) args[0]).doubleValue();
        double end   = ((Number) args[1]).doubleValue();
        double step  = ((Number) args[2]).doubleValue();
        for (double n = start; n < end; n += step) {
          result.add(n);
        }
      }
    }
    return result;
  }
}
