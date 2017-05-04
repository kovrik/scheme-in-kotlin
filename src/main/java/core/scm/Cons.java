package core.scm;

import core.exceptions.WrongTypeException;

import java.util.*;

import static core.writer.Writer.write;

// TODO Separate class for Proper and Improper Lists?
public class Cons<E> extends LinkedList<E> implements ICons, ITyped {

  /* Empty list constant */
  public static final Cons EMPTY = new Cons() {
    @Override public boolean isList() { return true; }
    @Override public Type getType() { return Type.LIST; }
  };

  private boolean isList;

  private Cons() {
    super();
    isList = true;
  }

  private Cons(Collection<? extends E> c) {
    super(c);
    isList = true;
  }

  private Cons(E car, E cdr) {
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
      throw new WrongTypeException("car", Type.PAIR.getName(), EMPTY);
    }
    return getFirst();
  }

  @Override
  public Object cdr() {
    return isList ? subList(1, size()) : getLast();
  }

  /* Convert list to improper list (dotted pair, cons cells) */
  public Cons<E> toCons() {
    if (!isList()) {
      return this;
    }
    /* Cons backwards */
    E last = get(size() - 1);
    E beforeLast = get(size() - 2);
    Cons<E> cons = cons(beforeLast, last);
    for (int n = size() - 3; n >= 0; n--) {
      cons = cons(get(n), (E)cons);
    }
    return cons;
  }

  @Override
  public Type getType() {
    return isList ? Type.LIST : Type.PAIR;
  }

  @Override
  public String toString() {
    return toString(this);
  }

  public static <E> Cons<E> cons(E car, E cdr) {
    if (car == null && cdr == null) {
      return EMPTY;
    }
    return (cdr == null) ? new Cons(car, EMPTY) : new Cons<>(car, cdr);
  }

  public static <E> Cons<E> list() {
    return new Cons<>();
  }

  public static <E> Cons<E> list(E e) {
    Cons<E> list = new Cons<>();
    list.add(e);
    return list;
  }

  public static <E> Cons<E> list(E e1, E e2) {
    Cons<E> list = new Cons<>();
    list.add(e1);
    list.add(e2);
    return list;
  }

  public static <E> Cons<E> list(E e1, E e2, E e3) {
    Cons<E> list = new Cons<>();
    list.add(e1);
    list.add(e2);
    list.add(e3);
    return list;
  }

  public static <E> Cons<E> list(E... elements) {
    return (elements == null || elements.length == 0) ? EMPTY : list(Arrays.asList(elements));
  }

  public static <E> Cons<E> list(Collection<? extends E> c) {
    if (c == null || c.isEmpty()) { return EMPTY; }
    return new Cons<>(c);
  }

  /* Return true if o is a List or Cons and a list */
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
      while (cdr instanceof Cons) {
        sb.append(" ").append(write(((Cons) cdr).getFirst()));
        cdr = ((Cons)cdr).getLast();
      }
      /* Dotted notation */
      sb.append(" . ").append(write(cdr));
    } else {
      /* List */
      for (int i = 0; i < list.size() - 1; i++) {
        Object e = list.get(i);
        sb.append(e == list ? "(this list)" : write(e));
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
    /* Two empty lists are always equal */
    if (size() == 0 && ((List) o).size() == 0) return true;
    if (this.size() != ((List)o).size()) return false;
    /* Improper lists are not equal to Proper lists, even if they have the same elements */
    if ((o instanceof Cons) && (isList != ((Cons) o).isList)) return false;
    Iterator<E> thisIterator = this.iterator();
    Iterator otherIterator = ((List) o).iterator();
    while (thisIterator.hasNext() && otherIterator.hasNext()) {
      Object thisNext = thisIterator.next();
      Object oNext = otherIterator.next();
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
