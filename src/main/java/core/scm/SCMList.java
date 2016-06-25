package core.scm;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

public class SCMList<E> extends LinkedList<E> {

  public static final SCMList EMPTY_LIST = new SCMList();

//  public SCMList() {
//    super();
//  }

  public SCMList(Collection<? extends E> c) {
    super(c);
  }

  public SCMList(E... elements) {
    super(Arrays.asList(elements));
  }

  public SCMList(E e) {
    super();
    add(e);
  }

  @Override
  public String toString() {
    Iterator<E> it = iterator();
    if (!it.hasNext()) {
      return "()";
    }
    StringBuilder sb = new StringBuilder();
    sb.append('(');
    for (;;) {
      E e = it.next();
      if (e == this) {
        sb.append("(this List)");
      } else if (e instanceof String) {
        sb.append('"').append(e).append('"');
      } else {
        sb.append(e);
      }
      if (!it.hasNext()) {
        return sb.append(')').toString();
      }
      sb.append(' ');
    }
  }
}
