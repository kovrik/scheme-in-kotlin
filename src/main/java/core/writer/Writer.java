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
    if (o == null)                 return "nil";
    if (o instanceof Boolean)      return (Boolean) o ? "#t" : "#f";
    if (o instanceof Symbol)       return write((Symbol) o);
    if (o instanceof Class)        return "#<class:" + ((Class) o).getName() + ">";
    if (o instanceof List)         return write((List) o);
    if (o instanceof Number)       return write((Number) o);
    if (o instanceof CharSequence) return write((CharSequence) o);
    if (o instanceof Character)    return write((Character) o);
    if (o instanceof Pattern)      return write((Pattern) o);
    if (o instanceof Throwable)    return write((Throwable) o);
    if (o instanceof Map)          return write((Map) o);
    if (o instanceof Set)          return write((Set) o);
    return o.toString();
  }

  public static String writeClass(Class clazz) {
    Type type = Type.valueOf(clazz);
    return type != null ? type.getName() : clazz.getSimpleName();
  }

  private static String write(Pattern pattern) {
    return "#\"" + pattern + "\"";
  }

  private static String write(List list) {
    return Cons.toString(list);
  }

  private static String write(Symbol symbol) {
    return symbol.isEscape() ? '|' + symbol.toString() + '|' : symbol.toString();
  }

  private static String write(Number number) {
    if (number instanceof Double) {
      if (Double.isNaN(number.doubleValue())) {
        return "+nan.0";
      } else if (number.equals(Double.POSITIVE_INFINITY)) {
        return "+inf.0";
      } else if (number.equals(Double.NEGATIVE_INFINITY)) {
        return "-inf.0";
      }
    }
    if (number instanceof Float) {
      if (Float.isNaN(number.floatValue())) {
        return "+nan.0";
      } else if (number.equals(Float.POSITIVE_INFINITY)) {
        return "+inf.0";
      } else if (number.equals(Float.NEGATIVE_INFINITY)) {
        return "-inf.0";
      }
    }
    return number.toString();
  }

  private static String write(CharSequence cs) {
    /* Unescape Strings */
    int length = cs.length();
    StringBuilder sb = new StringBuilder(length + 2);
    sb.append('"');
    for (int i = 0; i < length; i++) {
      char c = cs.charAt(i);
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

  private static String write(Character ch) {
    /* Check named characters */
    String codepoint = CODEPOINTS.get(ch);
    return codepoint == null ? "#\\" + ch : "#\\" + codepoint;
  }

  private static String write(Map<Object, Object> map) {
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

  private static String write(Set<Object> set) {
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

  private static String write(Throwable t) {
    if (t instanceof ExInfoException) {
      return t.toString();
    }
    return "#<error:" + t.getClass().getName() + ":" + (t.getMessage() == null ? "" : t.getMessage()) + ">";
  }
}
