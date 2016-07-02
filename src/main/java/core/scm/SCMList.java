package core.scm;

import java.util.*;
import java.util.function.UnaryOperator;

public class SCMList<E> extends LinkedList<E> {

  // TODO Make it a Singleton
  public static final SCMList NIL = new SCMList<Object>() {
    @Override
    public Object set(int index, Object element) {
      throw new UnsupportedOperationException();
    }
    @Override
    public void add(int index, Object element) {
      throw new UnsupportedOperationException();
    }
    @Override
    public Object remove(int index) {
      throw new UnsupportedOperationException();
    }
    // FIXME Why not working?
//    @Override
//    public boolean addAll(int index, Collection c) {
//      throw new UnsupportedOperationException();
//    }
    public void replaceAll(UnaryOperator operator) {
      throw new UnsupportedOperationException();
    }
    public void sort(Comparator c) {
      throw new UnsupportedOperationException();
    }
    // TODO @Override ListIterator?
  };

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
