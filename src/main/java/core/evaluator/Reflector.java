package core.evaluator;

import core.scm.SCMSymbol;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class Reflector {

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

  private Method getMethod(Class<?> clazz, String name, Object[] args, Class<?>[] parameterTypes) {
    try {
      return clazz.getMethod(name, parameterTypes);
    } catch (NoSuchMethodException e) {
      // no exact match found, try to find inexact match
      // FIXME Workaround: save state before downcasting
      Object[] argsOld = Arrays.copyOf(args, args.length);
      Class<?>[] paramsOld = Arrays.copyOf(parameterTypes, parameterTypes.length);
      downcastArgs(args, parameterTypes);
      try {
        return clazz.getMethod(name, parameterTypes);
      } catch (NoSuchMethodException ex) {
        try {
          // FIXME Workaround: restore previous state
          // restore saved state
          System.arraycopy(argsOld, 0, args, 0, argsOld.length);
          System.arraycopy(paramsOld, 0, parameterTypes, 0, paramsOld.length);
          // TODO Check if implemented properly
          castToObject(parameterTypes);
          return clazz.getMethod(name, parameterTypes);
        } catch (NoSuchMethodException ex2) {
          throw new RuntimeException(String.format("unable to find matching method %s in class %s",
                                                   name, clazz.getName()));
        }
      }
    }
  }

  private Constructor getConstructor(Class<?> clazz, Object[] args, Class<?>[] parameterTypes) {
    try {
      return clazz.getConstructor(parameterTypes);
    } catch (NoSuchMethodException e) {
      // no exact match found, try to find inexact match
      downcastArgs(args, parameterTypes);
      try {
        return clazz.getConstructor(parameterTypes);
      } catch (NoSuchMethodException ex) {
        throw new RuntimeException(String.format("unable to find matching constructor for class %s",
                                                 clazz.getName()));
      }
    }
  }

  // TODO Other types: short, byte etc.?
  private void downcastArgs(Object[] args, Class<?>[] parameterTypes) {
    for (int i = 0; i < parameterTypes.length; i++) {
      Class<?> parameterType = parameterTypes[i];
      if (Number.class.isAssignableFrom(BOXED.getOrDefault(parameterType, parameterType))) {
        // cast to int
        parameterTypes[i] = int.class;
        args[i] = ((Number)args[i]).intValue();
      }
    }
  }

  private void castToObject(Class<?>[] parameterTypes) {
    for (int i = 0; i < parameterTypes.length; i++) {
      parameterTypes[i] = Object.class;
    }
  }

  private Class unboxIfPossible(Class<?> clazz) {
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

  // TODO java.math.BigDecimal and other non-java.lang.* classes
  private Class getClass(String name) {
    if (name.indexOf('.') == -1) {
      name = "java.lang." + name;
    }
    try {
      return Class.forName(name);
    } catch (ClassNotFoundException e) {
      throw new RuntimeException("class not found: " + name);
    }
  }

  public Object newInstance(String clazz, Object... args) {
    Class c = getClass(clazz);
    Class[] argTypes = new Class[args.length];
    for (int i = 0; i < args.length; i++) {
      argTypes[i] = unboxIfPossible(args[i].getClass());
    }
    try {
      return getConstructor(c, args, argTypes).newInstance(args);
    } catch (InstantiationException | InvocationTargetException e) {
      throw new RuntimeException(e.getMessage());
    } catch (IllegalAccessException e) {
      throw new RuntimeException(String.format("unable to access constructor for class %s", clazz));
    }
  }

  /* Java Interop: static fields */
  Object evalJavaStaticField(String s) {
    if (s.indexOf('/') > -1) {
      String[] classAndField = s.split("/");
      String className = classAndField[0];
      String field = classAndField[1];
      Class c = getClass(className);
      try {
        return c.getField(field).get(c);
      } catch (NoSuchFieldException e) {
        throw new RuntimeException(String.format("unable to find static field %s in class %s", field, className));
      } catch (IllegalAccessException e) {
        throw new RuntimeException(String.format("unable to access static field %s in class %s", field, className));
      }
    }
    throw new RuntimeException("undefined identifier: " + s);
  }

  Object evalJavaMethod(String method, Object[] args) {
    if (method.indexOf('.') == 0) {
      Object instance = args[0];
      args = Arrays.copyOfRange(args, 1, args.length);
      return evalJavaInstanceMethod(method, instance, args);
    } else if (method.indexOf('/') != -1) {
      return evalJavaStaticMethod(method, args);
    }
    throw new RuntimeException("undefined identifier: " + method);
  }

  /* Java Interop: instance method call */
  private Object evalJavaInstanceMethod(String m, Object instance, Object[] args) {
    String methodName = m.substring(1);
    Class<?> clazz;
    boolean isClass = false;
    if (instance instanceof SCMSymbol) {
      clazz = getClass(instance.toString());
      isClass = true;
    } else {
      clazz = instance.getClass();
    }
    Class[] argTypes = new Class[args.length];
    for (int i = 0; i < args.length; i++) {
      argTypes[i] = unboxIfPossible(args[i].getClass());
    }
    Method method = getMethod(isClass ? Class.class : clazz, methodName, args, argTypes);
    try {
      return upcastIfPossible(method.invoke(isClass ? clazz : instance, args));
    } catch (IllegalAccessException e) {
      throw new RuntimeException(String.format("unable to access method %s of %s", methodName, instance));
    } catch (InvocationTargetException e) {
      throw new RuntimeException("reflection exception");
    }
  }

  /* Java Interop: static method call */
  private Object evalJavaStaticMethod(String m, Object[] args) {
    String[] classAndMethod = m.split("/");
    String className = classAndMethod[0];
    String methodName = classAndMethod[1];
    Class clazz = getClass(className);
    Class[] argTypes = new Class[args.length];
    for (int i = 0; i < args.length; i++) {
      argTypes[i] = unboxIfPossible(args[i].getClass());
    }
    Method method = getMethod(clazz, methodName, args, argTypes);
    try {
      return upcastIfPossible(method.invoke(null, args));
    } catch (IllegalAccessException e) {
      throw new RuntimeException(String.format("unable to access static method %s of %s", methodName, clazz.getName()));
    } catch (InvocationTargetException e) {
      throw new RuntimeException("reflection exception");
    }
  }
}
