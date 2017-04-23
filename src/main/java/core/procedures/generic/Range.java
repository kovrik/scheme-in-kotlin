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
      long start = 0;
      long end = 0;
      long step = 1;
      if (args.length == 1) {
        end = ((Number) args[0]).longValue();
      } else if (args.length == 2) {
        start = ((Number) args[0]).longValue();
        end   = ((Number) args[1]).longValue();
      } else if (args.length == 3) {
        start = ((Number) args[0]).longValue();
        end   = ((Number) args[1]).longValue();
        step  = ((Number) args[2]).longValue();
      }
      if (step >= 0) {
        for (long n = start; n < end; n += step) {
          result.add(n);
        }
      } else {
        for (long n = start; n > end; n += step) {
          result.add(n);
        }
      }
    } else {
      double start = 0;
      double end = 0;
      double step = 1;
      if (args.length == 1) {
        end = ((Number) args[0]).doubleValue();
      } else if (args.length == 2) {
        start = ((Number) args[0]).doubleValue();
        end   = ((Number) args[1]).doubleValue();
      } else if (args.length == 3) {
        start = ((Number) args[0]).doubleValue();
        end   = ((Number) args[1]).doubleValue();
        step  = ((Number) args[2]).doubleValue();
      }
      if (step >= 0) {
        for (double n = start; n < end; n += step) {
          result.add(n);
        }
      } else {
        for (double n = start; n > end; n += step) {
          result.add(n);
        }
      }
    }
    return result;
  }
}
