package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;

public final class IndexOf extends AFn {

  public IndexOf() {
    super(new FnArgsBuilder().min(2).max(3).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "index-of";
  }

  @Override
  public Integer apply(Object... args) {
    if (!(args[1] instanceof CharSequence) && !(args[1] instanceof Character)) {
      throw new WrongTypeException(getName(), "String or Character", args[1]);
    }
    if (args.length == 3) {
      Object index = args[2];
      if (!Utils.INSTANCE.isReal(index)) {
        throw new WrongTypeException(getName(), "Real", index);
      }
      if (args[1] instanceof Character) {
        return args[0].toString().indexOf((char)args[1], ((Number)index).intValue());
      }
      return args[0].toString().indexOf(args[1].toString(), ((Number)index).intValue());
    }
    if (args[1] instanceof Character) {
      return args[0].toString().indexOf((char)args[1]);
    }
    return args[0].toString().indexOf(args[1].toString());
  }
}
