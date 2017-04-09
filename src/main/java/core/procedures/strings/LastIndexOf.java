package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.NumberUtils;

public final class LastIndexOf extends AFn {

  public LastIndexOf() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(3).mandatoryArgsTypes(new Class[]{CharSequence.class}));
  }

  @Override
  public String getName() {
    return "last-index-of";
  }

  @Override
  public Integer apply(Object... args) {
    if (!(args[1] instanceof CharSequence) && !(args[1] instanceof Character)) {
      throw new WrongTypeException(getName(), "String or Character", args[1]);
    }
    if (args.length == 3) {
      Object index = args[2];
      if (!NumberUtils.isReal(index)) {
        throw new WrongTypeException(getName(), "Real", index);
      }
      if (args[1] instanceof Character) {
        return args[0].toString().lastIndexOf((char)args[1], ((Number)index).intValue());
      }
      return args[0].toString().lastIndexOf(args[1].toString(), ((Number)index).intValue());
    }
    if (args[1] instanceof Character) {
      return args[0].toString().lastIndexOf((char)args[1]);
    }
    return args[0].toString().lastIndexOf(args[1].toString());
  }
}
