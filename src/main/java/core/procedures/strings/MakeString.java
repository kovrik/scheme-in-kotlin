package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableString;

@FnArgs(isVariadic = true, args = {Long.class})
public class MakeString extends AFn {

  @Override
  public String getName() {
    return "make-string";
  }

  @Override
  public SCMMutableString invoke(Object... args) {
    Long s = (Long)args[0];
    if (s < 0) {
      throw new IllegalArgumentException(String.format("Size value is out of range in `%s`", getName()));
    }
    if (args.length > 2) {
      throw new ArityException(args.length, getName());
    }
    Object c = (args.length == 1) ? Character.MIN_VALUE : args[1];
    if (!(c instanceof Character)) {
      throw new WrongTypeException("Character", c);
    }
    SCMMutableString string = new SCMMutableString();
    for (long i = 0; i < s; i++) {
      string.append(c);
    }
    return string;
  }
}
