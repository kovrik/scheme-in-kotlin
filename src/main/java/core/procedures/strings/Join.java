package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Collection;
import java.util.Iterator;

public final class Join extends AFn {

  public Join() {
    super(new FnArgsBuilder().min(1).max(2).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "join";
  }

  @Override
  public String apply(Object... args) {
    if (args.length == 1) {
      return args[0].toString();
    }
    String separator = args[0].toString();
    Object col = args[1];
    if (col instanceof Collection) {
      StringBuilder sb = new StringBuilder();
      Collection coll = (Collection) col;
      int length = coll.size();
      if (length == 0) {
        return "";
      }
      int i = 0;
      Iterator iterator = coll.iterator();
      while (iterator.hasNext() && i < length - 1) {
        i += 1;
        sb.append(iterator.next()).append(separator);
      }
      return sb.append(iterator.next()).toString();
    }
    if (col instanceof CharSequence) {
      CharSequence cs = (CharSequence) col;
      int length = cs.length();
      if (length == 0) {
        return "";
      }
      StringBuilder sb = new StringBuilder();
      for (int i = 0; i < length - 1; i++) {
        sb.append(cs.charAt(i)).append(separator);
      }
      return sb.append(cs.charAt(cs.length() - 1)).toString();
    }
    throw new WrongTypeException(getName(), "List or Vector or String", col);
  }
}
