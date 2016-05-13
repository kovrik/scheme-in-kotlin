package main.core.ast;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

public class SCMList<E> extends LinkedList<E> {

  public SCMList() {
    super();
  }

  public SCMList(Collection<? extends E> c) {
    super(c);
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
      sb.append(e == this ? "(this List)" : e);
      if (! it.hasNext()) {
        return sb.append(')').toString();
      }
      sb.append(' ');
    }
  }
}
