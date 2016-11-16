package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMString;

public class StringProc extends AFn {

  @Override
  public String getName() {
    return "string";
  }

  @Override
  public SCMString invoke(Object... args) {
    if (args.length == 0) {
      return new SCMString();
    }
    SCMString string = new SCMString(args.length);
    for (Object c : args) {
      if (!(c instanceof Character)) {
        throw new WrongTypeException("Character", c);
      }
      string.append(c);
    }
    return string;
  }
}
