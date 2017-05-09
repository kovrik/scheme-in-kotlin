package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.vectors.Vec;
import core.scm.Vector;

import java.util.List;

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
    if (col instanceof Vector) {
      StringBuilder sb = new StringBuilder();
      Vector vec = (Vector) col;
      int length = vec.length();
      if (length == 0) {
        return "";
      }
      for (int i = 0; i < length - 1; i++) {
        sb.append(vec.get(i)).append(separator);
      }
      return sb.append(vec.get(length - 1)).toString();
    }
    if (col instanceof List) {
      List list = (List) col;
      int size = list.size();
      if (size == 0) {
        return "";
      }
      StringBuilder sb = new StringBuilder();
      for (int i = 0; i < size - 1; i++) {
        sb.append(list.get(i)).append(separator);
      }
      return sb.append(list.get(list.size() - 1)).toString();
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
