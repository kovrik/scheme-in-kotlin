package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.utils.NumberUtils;

import java.text.ParseException;

public class StringToNumber extends AFn {

  @Override
  public String getName() {
    return "string->number";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length < 1 || args.length > 2) {
      throw new ArityException(args.length, getName());
    }
    Object o = args[0];
    if (!(o instanceof String)) {
      throw new WrongTypeException("String", o);
    }

    String arg = (String)o;
    /* Check if we should override optional radix */
    // FIXME (string->number "1234#d") + exactness

    boolean override = false;
    int radix = 10;
    if (arg.contains("#b")) {
      radix = 2;
      override = true;
      arg = arg.replace("#b", "");
    } else if (arg.contains("#o")) {
      radix = 8;
      override = true;
      arg = arg.replace("#o", "");
    } else if (arg.contains("#d")) {
      radix = 10;
      override = true;
      arg = arg.replace("#d", "");
    } else if (arg.contains("#x")) {
      radix = 16;
      override = true;
      arg = arg.replace("#x", "");
    }

    /* Get default (optional) radix if present */
    if (args.length == 2) {
      Object o1 = args[1];
      if (!(o1 instanceof Long)) {
        throw new WrongTypeException("Integer", o);
      }
      int optRadix = ((Long)o1).intValue();
      if (optRadix < 2 || optRadix > 16) {
        throw new IllegalArgumentException("string->number: expected radix from 2 to 16!");
      }
      if (!override) {
        radix = optRadix;
      }
    }

    /* Read number */
    try {
      Object result = NumberUtils.preProcessNumber(arg, 'e', radix);
      if (result instanceof Number) {
        return result;
      }
    } catch (ParseException e) {
      // ignore
    }
    return SCMBoolean.FALSE;
  }
}