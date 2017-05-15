package core.scm;

import core.writer.Writer;

// TODO implement proper interfaces, refactor
public class MapEntry extends MutableVector {

  public MapEntry(Object key, Object value) {
    super(key, value);
  }

  public Object getKey() {
    return get(0);
  }

  public Object getValue() {
    return get(1);
  }

  @Override
  public String getName() {
    return "map entry";
  }

  @Override
  public String toString() {
    return "[" + Writer.write(getKey()) + " " + Writer.write(getValue()) + "]";
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    MapEntry that = (MapEntry) o;
    if (getKey() != null ? !getKey().equals(that.getKey()) : that.getKey() != null) return false;
    return getValue() != null ? getValue().equals(that.getValue()) : that.getValue() == null;
  }

  @Override
  public int hashCode() {
    int result = getKey() != null ? getKey().hashCode() : 0;
    result = 31 * result + (getValue() != null ? getValue().hashCode() : 0);
    return result;
  }
}
