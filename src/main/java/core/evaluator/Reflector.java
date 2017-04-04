package core.evaluator;

import core.exceptions.IllegalSyntaxException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Reflector {

  private static final Map<Class, Class> UNBOXED = new HashMap<>();
  static {
    UNBOXED.put(Byte.class,      byte.class);
    UNBOXED.put(Short.class,     short.class);
    UNBOXED.put(Integer.class,   int.class);
    UNBOXED.put(Long.class,      long.class);
    UNBOXED.put(Float.class,     float.class);
    UNBOXED.put(Double.class,    double.class);
    UNBOXED.put(Character.class, char.class);
    UNBOXED.put(Boolean.class,   boolean.class);
  }

  private static final Map<Class, Class> BOXED = new HashMap<>();
  static {
    for (Map.Entry<Class, Class> entry : UNBOXED.entrySet()) {
      BOXED.put(entry.getValue(), entry.getKey());
    }
  }

  // TODO Overloaded method resolution
  private Method getMethod(Class clazz, String name, Object[] args, Class<?>[] parameterTypes) {
    try {
      return clazz.getMethod(name, parameterTypes);
    } catch (NoSuchMethodException e) {
      // FIXME Generify and optimize
      // no exact match found, try to find inexact match
      for (int i = 0; i < parameterTypes.length; i++) {
        Class<?> parameterType = parameterTypes[i];
        if (Number.class.isAssignableFrom(BOXED.getOrDefault(parameterType, parameterType))) {
          // cast to int
          parameterTypes[i] = int.class;
          args[i] = ((Number)args[i]).intValue();
        } else {
          parameterTypes[i] = Object.class;
        }
      }
      try {
        return clazz.getMethod(name, parameterTypes);
      } catch (NoSuchMethodException ex) {
        throw new IllegalSyntaxException(String.format("method %s%s not found in class %s", name,
                                                       Arrays.toString(parameterTypes), clazz.getName()));
      }
    }
  }

  private Class unboxIfPossible(Class clazz) {
    return UNBOXED.getOrDefault(clazz, clazz);
  }

  private Object upcastIfPossible(Object value) {
    if (value instanceof Integer || value instanceof Short || value instanceof Byte) {
      return ((Number)value).longValue();
    } else if (value instanceof Float) {
      return ((Number)value).doubleValue();
    }
    return value;
  }

  private Class getClass(String name) {
    if (name.indexOf('.') == -1) {
      name = "java.lang." + name;
    }
    try {
      return Class.forName(name);
    } catch (ClassNotFoundException e) {
      throw new IllegalSyntaxException("class not found: " + name);
    }
  }

  Object evalJavaStaticField(String s) {
    /* Java Interop: static fields */
    if (s.indexOf('/') > -1) {
      String[] classAndField = s.split("/");
      String className = classAndField[0];
      String field = classAndField[1];
      Class c = getClass(className);
      try {
        return c.getField(field).get(c);
      } catch (NoSuchFieldException e) {
        throw new IllegalSyntaxException(String.format("unable to find static field %s in class %s", field, className));
      } catch (IllegalAccessException e) {
        throw new IllegalSyntaxException(String.format("unable to access static field %s in class %s", field, className));
      }
    }
    throw new IllegalArgumentException("undefined identifier: " + s);
  }

  // TODO Move reflection to a separate class
  // TODO get instance field value
  Object evalJavaMethod(List<Object> sexp) {
    Object op = sexp.get(0);
    /* Java Interop: instance method call */
    String m = op.toString();
    if (m.indexOf('.') == 0) {
      String methodName = m.substring(1);
      Object o = sexp.get(1);
      String name = o.toString();
      Class<?> clazz;
      boolean isClass = false;
      if (!name.isEmpty() && Character.isUpperCase(name.substring(name.lastIndexOf('.') + 1).charAt(0))) {
        clazz = getClass(name);
        isClass = true;
      } else {
        clazz = o.getClass();
      }
      Object[] args = sexp.subList(2, sexp.size()).toArray();
      Class[] argTypes = new Class[args.length];
      for (int i = 0; i < args.length; i++) {
        argTypes[i] = unboxIfPossible(args[i].getClass());
      }
      Method method = getMethod(isClass ? Class.class : clazz, methodName, args, argTypes);
      try {
        return upcastIfPossible(method.invoke(isClass ? clazz : o, args));
      } catch (IllegalAccessException e) {
        throw new IllegalSyntaxException(String.format("unable to access method %s of %s", methodName, o));
      } catch (InvocationTargetException e) {
        e.printStackTrace();
      }
    }
    /* Java Interop: static method call */
    if (m.indexOf('/') != -1) {
      String[] classAndMethod = m.split("/");
      String className = classAndMethod[0];
      String methodName = classAndMethod[1];
      Class clazz = getClass(className);
      Object[] args = sexp.subList(1, sexp.size()).toArray();
      Class[] argTypes = new Class[args.length];
      for (int i = 0; i < args.length; i++) {
        argTypes[i] = unboxIfPossible(args[i].getClass());
      }
      Method method = getMethod(clazz, methodName, args, argTypes);
      try {
        return upcastIfPossible(method.invoke(null, args));
      } catch (IllegalAccessException e) {
        throw new IllegalSyntaxException(String.format("unable to access static method %s of %s", methodName, clazz.getName()));
      } catch (InvocationTargetException e) {
        e.printStackTrace();
      }
    }
    throw new IllegalArgumentException("undefined identifier: " + op);
  }
}
