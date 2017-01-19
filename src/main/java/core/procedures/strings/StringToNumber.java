package core.procedures.strings;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.reader.parsers.Result;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.utils.NumberUtils;

import java.util.List;

import static core.utils.NumberUtils.EXACTNESS_RADIX;
import static core.utils.NumberUtils.RADIX_EXACTNESS;

@FnArgs(minArgs = 1, maxArgs = 2, mandatoryArgsTypes = {String.class}, restArgsType = {SCMClass.ExactPositiveInteger.class})
public final class StringToNumber extends AFn {

  @Override
  public String getName() {
    return "string->number";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length > 2) {
      throw new ArityException(args.length, getName());
    }
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
        throw new IllegalArgumentException("string->number: expected radix from 2 to 16!");
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