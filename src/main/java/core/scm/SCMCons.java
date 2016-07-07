package core.scm;

import java.util.*;

public class SCMCons<E> extends LinkedList<E> implements ICons {

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

  private boolean isList = false;

  private SCMCons() {
    super();
  }

  private SCMCons(E car, E cdr) {
    super();
    add(car);
    if (cdr instanceof SCMCons) {
      SCMCons cdrcons = (SCMCons) cdr;
      if (cdrcons.isList()) {
        setList(true);
        addAll(cdrcons);
      } else {
        add(cdr);
      }
    } else if (cdr instanceof List) {
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
    Iterator it = iterator();
    if (!isList) {
      StringBuilder cons = new StringBuilder();
      cons.append("(");
      cons.append(getFirst());
      Object cdr = getLast();
      if ((cdr instanceof SCMCons) && !((SCMCons)cdr).isList) {
        Object current = cdr;
        while ((current instanceof SCMCons) && !((SCMCons)current).isList) {
          cons.append(" ").append(((SCMCons) current).getFirst());
          current = ((SCMCons)current).getLast();
        }
        if (!NIL.equals(current)) {
          if (!isList()) {
            cons.append(" .");
          }
          cons.append(" ").append(current);
        }
      } else {
        /* Dotted notation */
        cons.append(" . ");
        cons.append(cdr);
      }
      cons.append(")");
      return cons.toString();
    }

    /* List */
    StringBuilder sb = new StringBuilder();
    sb.append('(');
    boolean first = true;
    for (;;) {
      Object e = it.next();
      if (e == this) {
        sb.append("(this Cons)");
      } else {
        if (!first) {
          sb.append(' ');
        } else {
          first = false;
        }
        sb.append(e);
      }
      if (!it.hasNext()) {
        return sb.append(')').toString();
      }
    }
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
    SCMCons list = new SCMCons();
    list.setList(true);
    list.addAll(Arrays.asList(elements));
    return list;
  }

  public static <E> SCMCons<E> list(List<E> list) {
    if (list == null || list.isEmpty()) {
      return NIL;
    }
    if ((list instanceof SCMCons) && !NIL.equals(list.get(list.size() - 1))) {
      SCMCons result = SCMCons.list();
      result.add(list);
      return result;
    } else {
      SCMCons<E> result = new SCMCons<E>();
      result.setList(true);
      result.addAll(list);
      return result;
    }
  }
}
