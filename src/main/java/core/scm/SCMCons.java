package core.scm;

import core.exceptions.WrongTypeException;

import java.util.*;

import static core.writer.Writer.write;

public class SCMCons<E> extends LinkedList<E> implements ICons, ISCMClass {

  /* Nil constant: empty list, but not a pair */
  public static final SCMCons NIL = new SCMCons() {
    @Override
    public boolean isList() { return true; }
    @Override
    public boolean isPair() { return false; }
    @Override
    public boolean isNull() { return true; }
    @Override
    public Object car() { throw new WrongTypeException("Pair", NIL); }
    @Override
    public Object cdr() { throw new WrongTypeException("Pair", NIL); }
    @Override
    public SCMClass getSCMClass() { return SCMClass.NIL; }
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

  @Override
  public boolean isList() {
    return isList;
  }

  @Override
  public boolean isPair() {
    return !NIL.equals(this);
  }

  @Override
  public boolean isNull() {
    return NIL.equals(this);
  }

  public void setList(boolean list) {
    isList = list;
  }

  @Override
  public E car() {
    if (isEmpty()) {
      throw new WrongTypeException("Pair", NIL);
    }
    return getFirst();
  }

  @Override
  public Object cdr() {
    if (isList) {
      return subList(1, size());
    } else {
      return getLast();
    }
  }

  public void setCdr(Object cdr) {
    if (isEmpty()) {
      throw new WrongTypeException("Pair", NIL);
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
  public SCMClass getSCMClass() {
    return SCMClass.PAIR;
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
    return ((o instanceof List) && !(o instanceof ICons)) || ((o instanceof ICons) && ((ICons)o).isList());
  }

  public static boolean isPair(Object o) {
    return (o instanceof List) && !(((List)o).isEmpty());
  }

  // TODO Move to Writer?
  /* Use this method to print all lists */
  public static String toString(List list) {

    if (list.isEmpty()) {
      return "()";
    }
    /* Cons cell */
    if (!isList(list)) {
      StringBuilder cons = new StringBuilder();
      cons.append("(").append(write(list.get(0)));
      Object cdr = list.get(list.size() - 1);
      while (cdr instanceof SCMCons) {
        cons.append(" ").append(write(((SCMCons) cdr).getFirst()));
        cdr = ((SCMCons)cdr).getLast();
      }
      /* Dotted notation */
      cons.append(" . ").append(write(cdr));
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
        sb.append(write(e));
      }
    }
    return sb.append(')').toString();
  }

  /* Non-recursively flatten a list (or a chain of conses) */
  public static <E> List<E> flatten(List<E> list) {
    List<E> result = new ArrayList<E>();
    LinkedList<E> queue = new LinkedList<E>(list);
    while (!queue.isEmpty()) {
      E e = queue.remove();
      if (e instanceof List) {
        queue.addAll(0, (List)e);
      } else {
        result.add(e);
      }
    }
    return result;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null) return false;
    if (!(o instanceof List)) return false;
    /* Two empty lists (NILs) are equal */
    if ((o instanceof SCMCons) && (((SCMCons) o).isEmpty()) && (this.isEmpty())) return true;
    if ((o instanceof SCMCons) && (isList != ((SCMCons) o).isList)) return false;
    if (this.size() != ((List)o).size()) return false;
    Iterator<E> thisIterator = this.iterator();
    Iterator oIterator = ((List) o).iterator();
    while (thisIterator.hasNext() && oIterator.hasNext()) {
      Object thisNext = thisIterator.next();
      Object oNext = oIterator.next();
      if (!thisNext.equals(oNext)) {
        return false;
      }
    }
    return true;
  }

  @Override
  public int hashCode() {
    int result = super.hashCode();
    result = 31 * result + (isList ? 1 : 0);
    return result;
  }
}
