package core.scm;

import java.util.*;
import java.util.function.UnaryOperator;

@Deprecated
public class SCMList<E> extends LinkedList<E> implements IList {

  public static final SCMList NIL = new SCMList<Object>() {
    public Object car() {
      throw new IllegalArgumentException("Wrong argument type. Expected: Pair, actual: '()");
    }

    public Object cdr() {
      throw new IllegalArgumentException("Wrong argument type. Expected: Pair, actual: '()");
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

  public Object cons(Object a) {
    SCMList list = new SCMList();
    list.addAll(this);
    list.push(a);
    return list;
  }

  public Object car() {
    if (NIL.equals(this)) {
      throw new IllegalArgumentException("Wrong argument type. Expected: Pair, actual: '()");
    }
    return get(0);
  }

  public Object cdr() {
    if (size() == 1) {
      return NIL;
    } else {
      // FIX Should share tail, but return SCMList, not SubList!
      return new SCMList(subList(1, size()));
      /* Share tail */
//      return subList(1, size());
    }
  }

  public long length() {
    return this.size();
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

  public static SCMList list() {
    return NIL;
  }

  public static <E> SCMList list(E e) {
    SCMList list = new SCMList();
    list.add(e);
    return list;
  }

  public static <E> SCMList list(Collection<? extends E> c) {
    if (c == null || c.isEmpty()) {
      return NIL;
    }
    SCMList list = new SCMList();
    list.addAll(c);
    return list;
  }

  public static <E> SCMList list(E... elements) {
    if (elements == null || elements.length == 0) {
      return NIL;
    }
    SCMList list = new SCMList();
    list.addAll(Arrays.asList(elements));
    return list;
  }
}
