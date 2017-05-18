package core.procedures.generic;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.math.Addition;
import core.procedures.math.NumericalComparison;
import core.scm.BigRatio;
import core.scm.Type;
import core.scm.Cons;
import core.utils.Utils;

import java.math.BigDecimal;
import java.util.List;

public class Range extends AFn {

  public Range() {
    super(new FnArgsBuilder().min(0).max(3).rest(Type.Real.class).build());
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
      return Cons.EMPTY;
    }
    boolean fraction = args[0] instanceof BigRatio;
    if (args.length == 3) {
      fraction = fraction || args[2] instanceof BigRatio;
    }
    boolean big = args[0] instanceof BigDecimal;
    if (args.length == 2) {
      big = big || args[1] instanceof BigDecimal;
    }
    if (args.length == 3) {
      big = big || args[1] instanceof BigDecimal || args[2] instanceof BigDecimal;
    }
    if (fraction || big) {
      return range(args);
    }

    boolean exact = Utils.INSTANCE.isExactInteger(args[0]);
    if (args.length == 3) {
      exact = exact && Utils.INSTANCE.isExactInteger(args[2]);
    }
    Cons<Number> result = Cons.list();
    if (exact) {
      long start = 0;
      long end = 0;
      long step = 1;
      if (args.length == 1) {
        end = args[0] instanceof Double ? Math.round((Double) args[0]) : ((Number) args[0]).longValue();
      } else if (args.length == 2) {
        start = ((Number) args[0]).longValue();
        end = args[1] instanceof Double ? Math.round((Double) args[1]) : ((Number) args[1]).longValue();
      } else if (args.length == 3) {
        start = ((Number) args[0]).longValue();
        end = args[1] instanceof Double ? Math.round((Double) args[1]) : ((Number) args[1]).longValue();
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

  private List<Number> range(Object[] args) {
    Cons<Number> result = Cons.list();
    Number start = 0L;
    Number end = 0L;
    Number step = 1L;
    if (args.length == 1) {
      end   = (Number)args[0];
    } else if (args.length == 2) {
      start = (Number)args[0];
      end   = (Number)args[1];
    } else if (args.length == 3) {
      start = (Number)args[0];
      end   = (Number)args[1];
      step  = (Number)args[2];
    }
    Number cur = start;
    NumericalComparison pred = NumericalComparison.LESS;
    if (Utils.INSTANCE.isNegative(step)) {
      pred = NumericalComparison.GREATER;
    }
    while (pred.apply(cur, end)) {
      result.add(cur);
      cur = Addition.add(cur, step);
    }
    return result;
  }
}
