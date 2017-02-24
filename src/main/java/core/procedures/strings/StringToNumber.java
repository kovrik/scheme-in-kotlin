package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.reader.parsers.Result;
import core.scm.SCMClass;
import core.utils.NumberUtils;

import java.util.List;

import static core.utils.NumberUtils.EXACTNESS_RADIX;
import static core.utils.NumberUtils.RADIX_EXACTNESS;

public final class StringToNumber extends AFn {

  public StringToNumber() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(2)
                             .mandatoryArgsTypes(new Class[]{String.class})
                             .restArgsType(new Class[]{SCMClass.ExactPositiveInteger.class}));
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
    Result parse = EXACTNESS_RADIX.parse(number);
    if (parse.getType() == Result.Type.SUCCESS) {
      List<String> match = parse.getMatch();
      exactness = match.get(0).charAt(1);
      if (match.size() > 1) {
        override = true;
        radixChar = match.get(1).charAt(1);
      }
    } else {
      parse = RADIX_EXACTNESS.parse(number);
      if (parse.getType() == Result.Type.SUCCESS) {
        override = true;
        List<String> match = parse.getMatch();
        radixChar = match.get(0).charAt(1);
        if (match.size() > 1) {
          exactness = match.get(1).charAt(1);
        }
      }
    }
    number = parse.getRest();

    radixChar = (radixChar == null) ? 'd' : radixChar;
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
    Object result = NumberUtils.preProcessNumber(number, exactness, radix);
    if (result instanceof Number) {
      return result;
    }
    return Boolean.FALSE;
  }
}