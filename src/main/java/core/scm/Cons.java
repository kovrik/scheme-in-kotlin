package core.scm;

import java.util.List;
import java.util.ListIterator;

@Deprecated
public class Cons {

  public static final Cons NIL = new Cons(null, null);

  private Object car;
  private Object cdr;

  private Object last;

  private long length = 0;

  private Cons(Object car, Object cdr) {

    if (car == null && cdr == null) {
      /* NIL */
      this.car = null;
      this.cdr = null;
      this.last = null;
    } else {
      this.car = (car == null) ? NIL : car;
      this.cdr = (cdr == null) ? NIL : cdr;
      if (isNil(cdr)) {
        this.last = NIL;
        this.length = 1;
      } else {
        if (cdr instanceof Cons) {
          this.last = ((Cons) cdr).last;
          this.length = ((Cons) cdr).length + 1;
        } else {
          this.last = cdr;
        }
      }
    }
  }

  @Override
  public String toString() {
    /* NIL */
    if (isNil(this)) {
      return "()";
    }
    if (cdr == null || isNil(cdr)) {
      return "(" + car + ")";
    }
    if (cdr instanceof Cons) {
      Object current = cdr;
      StringBuilder sb = new StringBuilder();
      sb.append('(').append(car);

      while (isPair(current)) {
        sb.append(" ").append(((Cons) current).car);
        current = ((Cons) current).cdr;
      }
      if (!isNil(current)) {
        if (!isList(this)) {
          sb.append(" .");
        }
        sb.append(" ").append(current);
      }
      sb.append(')');
      return sb.toString();
    }
    return "(" + car + " . " + cdr + ")";
  }

  @Override
  public boolean equals(Object o) {

    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Cons cons = (Cons) o;
    if (car != null ? !car.equals(cons.car) : cons.car != null) {
      return false;
    }
    return cdr != null ? cdr.equals(cons.cdr) : cons.cdr == null;
  }

  @Override
  public int hashCode() {

    int result = car != null ? car.hashCode() : 0;
    result = 31 * result + (cdr != null ? cdr.hashCode() : 0);
    return result;
  }

  /**
   * Static helper methods
   */
  @Deprecated
  public static Cons cons(Object car, Object cdr) {
    if (car == null && cdr == null) {
      return NIL;
    }
    return new Cons(car, cdr);
  }

  @Deprecated
  public static Object car(Cons cons) {
    return cons.car;
  }

  @Deprecated
  public static Object cdr(Cons cons) {
    return cons.cdr;
  }

  @Deprecated
  public static boolean isNil(Object o) {
    return o == null || NIL.equals(o);
  }

  @Deprecated
  public static boolean isPair(Object o) {
    return (o instanceof Cons) && !NIL.equals(o);
  }

  @Deprecated
  public static boolean isList(Object o) {
    return (o instanceof Cons) && (NIL.equals(o) || NIL.equals(((Cons) o).last));
  }

  @Deprecated
  public static Cons list(Object... elements) {
    if (elements == null || elements.length == 0) {
      return NIL;
    }
    Cons list = cons(elements[elements.length - 1], NIL);
    for (int i = elements.length - 2; i >= 0; i--) {
      list = cons(elements[i], list);
    }
    return list;
  }

  @Deprecated
  public static Cons list(List list) {
    Cons result = NIL;
    if (list == null || list.isEmpty()) {
      return result;
    }
    ListIterator iterator = list.listIterator(list.size());
    while (iterator.hasPrevious()) {
      result = cons(iterator.previous(), result);
    }
    return result;
  }

  @Deprecated
  public static long length(Cons list) {
    return list.length;
  }
}
