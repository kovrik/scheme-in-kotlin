package core.scm;

import core.exceptions.WrongTypeException;
import core.utils.Utils;

/* Mutable Vector */
public class MutableVector extends Vector {

  public MutableVector() {
    super();
  }

  public MutableVector(int size, Object init) {
    super(size, init);
  }

  public MutableVector(Object... elements) {
    super((Object[])elements);
  }

  public MutableVector(Object e) {
    super(e);
  }

  public void set(int index, Object value) {
    if (size() <= index) throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", getName(), index));
    array[index] = value;
  }

  @Override
  public Object[] getArray() {
    return array;
  }

  @Override
  public Object[] toArray() {
    return array;
  }

  @Override
  public Object assoc(Object key, Object value) {
    if (!Utils.INSTANCE.isInteger(key)) {
      throw new WrongTypeException(getName(), Integer.class, key);
    }
    int i = ((Number) key).intValue();
    set(i, value);
    return this;
  }
}

