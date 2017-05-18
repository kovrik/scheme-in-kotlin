package core.scm;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;
import core.writer.Writer;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Objects;

// TODO implement List instead?
/* Immutable Vector */
public class Vector extends AFn implements Collection, IAssoc {

  /* Scheme Vector syntax */
//  private static final String OPEN = "#(";
//  private static final String CLOSE = ")";
  /* Alternative Clojure-style Vector syntax */
  private static final String OPEN = "[";
  private static final String CLOSE = "]";

  /* Contents of Vector: plain Java array */
  final Object[] array;

  public Vector() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.ExactNonNegativeInteger.class}).build());
    this.array = new Object[0];
  }

  public Vector(int size, Object init) {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.ExactNonNegativeInteger.class}).build());
    this.array = new Object[size];
    Arrays.fill(getArray(), init);
  }

  public Vector(Object... elements) {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.ExactNonNegativeInteger.class}).build());
    this.array = elements;
  }

  public Vector(Object e) {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.ExactNonNegativeInteger.class}).build());
    this.array = new Object[] {e};
  }

  @Override
  public Object apply1(Object arg) {
    int index = ((Number)arg).intValue();
    if (index >= array.length) {
      throw new IndexOutOfBoundsException(getName() + ": value out of range: " + index);
    }
    return array[index];
  }

  public Object get(int index) {
    return getArray()[index];
  }

  @Override
  public String getName() {
    return "vector";
  }

  @Override
  public String toString() {
    if (getArray().length == 0) {
      return OPEN + CLOSE;
    }
    StringBuilder sb = new StringBuilder();
    sb.append(OPEN);
    for (int i = 0; i < getArray().length; i++) {
      Object e = getArray()[i];
      sb.append(e == this ? "(this vector)" : Writer.write(e));
      if (i == getArray().length - 1) {
        return sb.append(CLOSE).toString();
      }
      sb.append(' ');
    }
    return sb.toString();
  }

  @Override
  public Iterator<Object> iterator() {
    return Arrays.asList(array).iterator();
  }

  @Override
  public int size() {
    return array.length;
  }

  @Override
  public boolean isEmpty() {
    return size() == 0;
  }

  @Override
  public boolean contains(Object o) {
    for (Object e : array) {
      if (Objects.equals(e, o)) {
        return true;
      }
    }
    return false;
  }

  public Object[] getArray() {
    return Arrays.copyOf(array, size());
  }

  @Override
  public Object[] toArray() {
    return Arrays.copyOf(array, size());
  }

  @Override
  public Object[] toArray(Object[] a) {
    return Arrays.copyOf(array, size());
  }

  @Override
  public boolean add(Object o) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean remove(Object o) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean addAll(Collection c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void clear() {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean retainAll(Collection c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean removeAll(Collection c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean containsAll(Collection col) {
    for (Object e : col) {
      if (!contains(e)) {
        return false;
      }
    }
    return true;
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(getArray());
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof Vector && Arrays.equals(getArray(), ((Vector) obj).getArray());
  }

  @Override
  public boolean containsKey(Object key) {
    if (!Utils.INSTANCE.isInteger(key)) {
      throw new WrongTypeException(getName(), Integer.class, key);
    }
    int i = ((Number) key).intValue();
    return size() > i;
  }

  @Override
  public MapEntry getEntry(Object key) {
    if (!Utils.INSTANCE.isInteger(key)) {
      throw new WrongTypeException(getName(), Integer.class, key);
    }
    int i = ((Number) key).intValue();
    return new MapEntry(i, get(i));
  }

  @Override
  public Object assoc(Object key, Object value) {
    throw new UnsupportedOperationException("assoc is not supported for immutable vector");
  }
}
