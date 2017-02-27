package core.scm;

import core.exceptions.WrongTypeException;

import java.util.*;

import static core.writer.Writer.write;

// TODO Separate class for Proper and Improper Lists
public class SCMCons<E> extends LinkedList<E> implements ICons, ISCMClass {

  /* Nil constant: empty list */
  public static final SCMCons NIL = new SCMCons() {
    @Override public boolean isList() { return true; }
    @Override public SCMClass getSCMClass() { return SCMClass.NIL; }
  };

  private boolean isList;

  private SCMCons() {
    super();
    isList = true;
  }

  private SCMCons(E car, E cdr) {
    super();
    add(car);
    isList = isList(cdr);
    if (isList) {
      /* cons becomes a list */
      addAll((List)cdr);
    } else {
      add(cdr);
    }
  }

  @Override
  public boolean isList() {
    return isList;
  }

  public void setIsList(boolean list) {
    isList = list;
  }

  @Override
  public E car() {
    if (isEmpty()) {
      throw new WrongTypeException("car", SCMClass.PAIR.getName(), NIL);
    }
    return getFirst();
  }

  @Override
  public Object cdr() {
    return isList ? subList(1, size()) : getLast();
  }

  /* Convert list to improper list (dotted pair, cons cells) */
  public SCMCons<E> toCons() {
    if (!isList()) {
      return this;
    }
    /* Cons backwards */
    E last = get(size() - 1);
    E beforeLast = get(size() - 2);
    SCMCons<E> cons = cons(beforeLast, last);
    for (int n = size() - 3; n >= 0; n--) {
      cons = cons(get(n), (E)cons);
    }
    return cons;
  }

  @Override
  public SCMClass getSCMClass() {
    return isList ? SCMClass.LIST : SCMClass.PAIR;
  }

  @Override
  public String toString() {
    return toString(this);
  }

  public static <E> SCMCons<E> cons(E car, E cdr) {
    if (car == null && cdr == null) {
      return NIL;
    }
    return (cdr == null) ? new SCMCons(car, NIL) : new SCMCons<>(car, cdr);
  }

  public static <E> SCMCons<E> list() {
    return new SCMCons<>();
  }

  public static <E> SCMCons<E> list(E... elements) {
    return (elements == null || elements.length == 0) ? NIL : list(Arrays.asList(elements));
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

  public static boolean isNull(Object object) {
    if (object instanceof List) {
      return ((List)object).isEmpty();
    }
    return object == null;
  }

  /* Use this method to print all lists */
  public static String toString(List list) {
    if (list.isEmpty()) {
      return "()";
    }
    /* Cons cell */
    StringBuilder sb = new StringBuilder();
    sb.append("(");
    if (!isList(list)) {
      sb.append(write(list.get(0)));
      Object cdr = list.get(list.size() - 1);
      while (cdr instanceof SCMCons) {
        sb.append(" ").append(write(((SCMCons) cdr).getFirst()));
        cdr = ((SCMCons)cdr).getLast();
      }
      /* Dotted notation */
      sb.append(" . ").append(write(cdr));
    } else {
      /* List */
      for (int i = 0; i < list.size() - 1; i++) {
        Object e = list.get(i);
        if (e == list) {
          sb.append("(this List)");
        } else {
          sb.append(write(e));
        }
        sb.append(' ');
      }
      sb.append(write(list.get(list.size() - 1)));
    }
    return sb.append(')').toString();
  }

  /* Non-recursively flatten a list (or a chain of conses) */
  public static <E> List<E> flatten(List<E> list) {
    List<E> result = new ArrayList<>();
    LinkedList<E> queue = new LinkedList<>(list);
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
    return 31 * super.hashCode() + (isList ? 1 : 0);
  }
}
