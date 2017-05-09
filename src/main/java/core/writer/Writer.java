package core.writer;

import core.exceptions.ExInfoException;
import core.reader.Reader;
import core.scm.Cons;
import core.scm.Symbol;
import core.scm.Type;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

public class Writer {

  private static final Map<Character, String> CODEPOINTS = new HashMap<>();
  static {
    Reader.NAMED_CHARS.forEach((key, value) -> CODEPOINTS.put(value, key));
  }

  private static final Map<Character, Character> UNESCAPED = new HashMap<>();
  static {
    UNESCAPED.put('\t', 't');
    UNESCAPED.put('\b', 'b');
    UNESCAPED.put('\r', 'r');
    UNESCAPED.put('\n', 'n');
    UNESCAPED.put('\f', 'f');
    UNESCAPED.put('\"', '"');
    UNESCAPED.put( '\\', '\\');
  }

  public static String write(Object o) {
    if (o == null) {
      return "nil";
    }
    if (o instanceof Boolean) {
      return (Boolean) o ? "#t" : "#f";
    }
    if (o instanceof Symbol) {
      return ((Symbol) o).isEscape() ? '|' + o.toString() + '|' : o.toString();
    }
    if (o instanceof Type) {
      return o.toString();
    }
    if (o instanceof Class) {
      return "#<class:" + ((Class)o).getName() + ">";
    }
    if (o instanceof List) {
      return Cons.toString((List) o);
    }
    if (o instanceof Double) {
      if (Double.isNaN(((Number) o).doubleValue())) {
        return "+nan.0";
      } else if (o.equals(Double.POSITIVE_INFINITY)) {
        return "+inf.0";
      } else if (o.equals(Double.NEGATIVE_INFINITY)) {
        return "-inf.0";
      }
      return o.toString();
    }
    if (o instanceof Number) {
      return o.toString();
    }
    if (o instanceof CharSequence) {
      /* Unescape Strings */
      int length = ((CharSequence) o).length();
      StringBuilder sb = new StringBuilder(length + 2);
      sb.append('"');
      for (int i = 0; i < length; i++) {
        char c = ((CharSequence) o).charAt(i);
        Character character = UNESCAPED.get(c);
        if (character == null) {
          sb.append(c);
        } else {
          sb.append('\\').append(character);
        }
      }
      sb.append('"');
      return sb.toString();
    }
    if (o instanceof Character) {
      /* Check named characters */
      String codepoint = CODEPOINTS.get(o);
      return codepoint == null ? "#\\" + o.toString() : "#\\" + codepoint;
    }
    if (o instanceof Pattern) {
      return "#\"" + o + "\"";
    }
    if (o instanceof Throwable) {
      if (o instanceof ExInfoException) {
        return o.toString();
      }
      return writeException((Throwable) o);
    }
    if (o instanceof Map) {
      return writeMap((Map)o);
    }
    if (o instanceof Set) {
      return writeSet((Set)o);
    }
    return o.toString();
  }

  public static String writeClass(Class clazz) {
    Type type = Type.valueOf(clazz);
    return type != null ? type.getName() : clazz.getSimpleName();
  }

  private static String writeMap(Map<Object, Object> map) {
    if (map.isEmpty()) {
      return  "{}";
    }
    StringBuilder sb = new StringBuilder().append('{');
    boolean first = true;
    for (Map.Entry<Object, Object> entry : map.entrySet()) {
      if (first) {
        first = false;
      } else {
        sb.append(", ");
      }
      Object key = entry.getKey();
      sb.append(key == map ? "(this hashmap)" : write(key));
      sb.append(' ');
      Object value = entry.getValue();
      sb.append(value == map? "(this hashmap)" : write(value));
    }
    return sb.append('}').toString();
  }

  private static String writeSet(Set<Object> set) {
    if (set.isEmpty()) {
      return  "#{}";
    }
    StringBuilder sb = new StringBuilder().append("#{");
    boolean first = true;
    for (Object e : set) {
      if (first) {
        first = false;
      } else {
        sb.append(' ');
      }
      sb.append(e == set ? "(this set)" : write(e));
    }
    return sb.append('}').toString();
  }

  private static String writeException(Throwable t) {
    return "#<error:" + t.getClass().getName() + ":" + (t.getMessage() == null ? "" : t.getMessage()) + ">";
  }
}
