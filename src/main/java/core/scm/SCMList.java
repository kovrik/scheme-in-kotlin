package core.scm;

import java.util.*;
import java.util.function.UnaryOperator;

public class SCMList<E> extends LinkedList<E> implements IPair {

  // TODO Make it a Singleton?
  // TODO Make it a separate class?
  public static final SCMList NIL = new SCMList<Object>() {
    public Object car() {
      throw new IllegalArgumentException("Wrong argument type. Expected: Pair, actual: '()");
    }

    public Object cdr() {
      throw new IllegalArgumentException("Wrong argument type. Expected: Pair, actual: '()");
    }

    public boolean isPair() {
      return false;
    }

    public boolean isNull() {
      return true;
    }

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
    @Override
    public boolean addAll(int index, Collection c) {
      throw new UnsupportedOperationException();
    }
    public void replaceAll(UnaryOperator operator) {
      throw new UnsupportedOperationException();
    }
    public void sort(Comparator c) {
      throw new UnsupportedOperationException();
    }
    // TODO @Override ListIterator?
  };

  public SCMList() {
    super();
  }

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

  public Object car() {
    return get(0);
  }

  public Object cdr() {
    return subList(1, size());
  }

  public boolean isPair() {
    return true;
  }

  public boolean isNull() {
    return false;
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
