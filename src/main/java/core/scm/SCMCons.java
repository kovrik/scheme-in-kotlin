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

    /* Add all elements only if it is a list (not cons) */
    if (((cdr instanceof List)  && !(cdr instanceof ICons)) ||
        ((cdr instanceof ICons) && ((ICons)cdr).isList())) {

      /* cons becomes a list */
      setList(true);
      addAll((List)cdr);
    } else {
      add(cdr);
    }
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
      /* FIXME set-cdr! won't work! */
      return subList(1, size());
    } else {
      return getLast();
    }
  }

  @Override
  public String toString() {

    if (isEmpty()) {
      return "()";
    }
    /* Cons cell */
    if (!isList) {
      StringBuilder cons = new StringBuilder();
      cons.append("(").append(getFirst());
      Object cdr = getLast();
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
    for (Object e : this) {
      if (!first) {
        sb.append(' ');
      } else {
        first = false;
      }
      if (e == this) {
        sb.append("(this Cons)");
      } else {
        sb.append(e);
      }
    }
    return sb.append(')').toString();
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
}
