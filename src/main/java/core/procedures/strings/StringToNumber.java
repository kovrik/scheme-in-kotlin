package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.utils.NumberUtils;

import static core.reader.Reader.isExactness;
import static core.reader.Reader.isRadix;

public final class StringToNumber extends AFn {

  public StringToNumber() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(2)
                             .mandatoryArgsTypes(new Class[]{String.class})
                             .restArgsType(SCMClass.ExactPositiveInteger.class));
  }

  @Override
  public String getName() {
    return "string->number";
  }

  @Override
  public Object apply(Object... args) {
    String number = args[0].toString();
    /* Check if we should override optional radix */
    /* Read radix and/or exactness and a number */
    boolean override = false;
    Character radixChar = null;
    Character exactness = null;
    String restNumber = number;
    while (restNumber.length() > 1 && restNumber.charAt(0) == '#') {
      char ch = restNumber.charAt(1);
      if (isExactness.test(ch)) {
        if (exactness != null) {
          return Boolean.FALSE;
        }
        exactness = ch;
        restNumber = restNumber.substring(2);
        continue;
      }
      if (isRadix.test(ch)) {
        if (radixChar != null) {
          return Boolean.FALSE;
        }
        radixChar = ch;
        restNumber = restNumber.substring(2);
        override = true;
        continue;
      }
      break;
    }
    if (restNumber.isEmpty()) {
      return Boolean.FALSE;
    }
    int radix = NumberUtils.getRadixByChar(radixChar);
    /* Get default (optional) radix if present */
    if (args.length == 2) {
      int optRadix = ((Long)args[1]).intValue();
      if (optRadix < 2 || optRadix > 16) {
        throw new IllegalArgumentException(getName() + ": expected radix from 2 to 16!");
      }
      if (!override) {
        radix = optRadix;
      }
    }

    /* Read number */
    Object result = NumberUtils.preProcessNumber(restNumber, exactness, radix);
    if (result instanceof Number) {
      return result;
    }
    return Boolean.FALSE;
  }
}