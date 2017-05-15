package core.scm;

import core.procedures.FnArgsBuilder;
import core.writer.Writer;

import java.util.Map;

public class MapEntry extends Vector implements Map.Entry {

  private final Object key;
  private final Object value;

  public MapEntry(Object key, Object value) {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.ExactNonNegativeInteger.class}).build());
    this.key = key;
    this.value = value;
  }

  @Override
  public Object getKey() {
    return key;
  }

  @Override
  public Object getValue() {
    return value;
  }

  @Override
  public Object setValue(Object value) {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getName() {
    return "map entry";
  }

  @Override
  public String toString() {
    return "[" + Writer.write(key) + " " + Writer.write(value) + "]";
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    MapEntry that = (MapEntry) o;
    if (key != null ? !key.equals(that.key) : that.key != null) return false;
    return value != null ? value.equals(that.value) : that.value == null;
  }

  @Override
  public int hashCode() {
    int result = key != null ? key.hashCode() : 0;
    result = 31 * result + (value != null ? value.hashCode() : 0);
    return result;
  }
}
