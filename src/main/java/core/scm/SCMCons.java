package core.scm;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

public class SCMCons<E> extends LinkedList<E> implements ICons {

  /* Empty list constant */
  public static final SCMCons NIL = new SCMCons() {
    @Override
    public boolean isList() {
      /* Nil is a list */
      return true;
    }

    @Override
    public boolean isPair() {
      /* Nil is not a pair */
      return false;
    }

    @Override
    public boolean isNull() {
      return true;
    }

    @Override
    public Object car() {
      throw new IllegalArgumentException("Wrong argument type. Expected: Pair, actual: ()");
    }

    @Override
    public Object cdr() {
      throw new IllegalArgumentException("Wrong argument type. Expected: Pair, actual: ()");
    }
  };

  /* By default every cons is a cons, not a list */
  private boolean isList = false;

  private SCMCons() {
    super();
  }

  private SCMCons(E car, E cdr) {
    super();
    add(car);
    setCdr(cdr);
  }

  public boolean isList() {
    return isList;
  }

  public boolean isPair() {
    return !NIL.equals(this);
  }

  public boolean isNull() {
    return NIL.equals(this);
  }

  public void setList(boolean list) {
    isList = list;
  }

  public E car() {
    if (isEmpty()) {
      throw new IllegalArgumentException("Wrong argument type. Expected: Pair, actual: ()");
    }
    return getFirst();
  }

  public Object cdr() {
    if (isList) {
      return subList(1, size());
    } else {
      return getLast();
    }
  }

  public void setCdr(Object cdr) {
    if (isEmpty()) {
      throw new IllegalArgumentException("Wrong argument type. Expected: Pair, actual: ()");
    }
    subList(1, size()).clear();

    /* Add all elements only if it is a list (not cons) */
    if (isList(cdr)) {
      /* cons becomes a list */
      setList(true);
      addAll((List)cdr);
    } else {
      setList(false);
      add((E)cdr);
    }
  }

  @Override
  public String toString() {
    return toString(this);
  }

  public static <E> SCMCons<E> cons(E car, E cdr) {
    if (car == null && cdr == null) {
      return NIL;
    }
    if (cdr == null) {
      return new SCMCons(car, NIL);
    }
    return new SCMCons<E>(car, cdr);
  }

  public static <E> SCMCons<E> list() {
    SCMCons<E> list = new SCMCons<E>();
    list.setList(true);
    return list;
  }

  public static <E> SCMCons<E> list(E... elements) {
    if (elements == null || elements.length == 0) {
      return NIL;
    }
    return list(Arrays.asList(elements));
  }

  public static <E> SCMCons<E> list(List<E> list) {
    if (list == null || list.isEmpty()) {
      return NIL;
    }
    SCMCons<E> result = list();
    result.addAll(list);
    return result;
  }

  /* Return true if o is a List or SCMCons and a list */
  public static boolean isList(Object o) {
    return ((o instanceof List)  && !(o instanceof ICons)) || ((o instanceof ICons) && ((ICons)o).isList());
  }

  public static boolean isPair(Object o) {
    return (o instanceof List) && !(((List)o).isEmpty());
  }

  /* Use this method to print all lists */
  public static String toString(List list) {

    if (list.isEmpty()) {
      return "()";
    }
    /* Cons cell */
    if (!isList(list)) {
      StringBuilder cons = new StringBuilder();
      cons.append("(").append(list.get(0));
      Object cdr = list.get(list.size() - 1);
      while (cdr instanceof SCMCons) {
        cons.append(" ").append(((SCMCons) cdr).getFirst());
        cdr = ((SCMCons)cdr).getLast();
      }
      /* Dotted notation */
      cons.append(" . ").append(cdr);
      return cons.append(")").toString();
    }
    /* List */
    StringBuilder sb = new StringBuilder();
    sb.append('(');
    boolean first = true;
    for (Object e : list) {
      if (!first) {
        sb.append(' ');
      } else {
        first = false;
      }
      if (e == list) {
        sb.append("(this List)");
      } else {
        sb.append(e);
      }
    }
    return sb.append(')').toString();
  }
}
