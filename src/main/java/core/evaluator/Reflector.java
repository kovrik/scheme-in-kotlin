package core.evaluator;

import core.exceptions.IllegalSyntaxException;
import core.exceptions.UndefinedIdentifierException;
import core.scm.Symbol;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

// TODO get instance fields? (.-instanceField instance)
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

  /* Some common classes that are not in java.lang. package could be resolved without package name */
  private static final Map<String, String> CLASS_PACKAGE_MAPPING = new HashMap<>();
  static {
    CLASS_PACKAGE_MAPPING.put("BigInteger", "java.math.BigInteger");
    CLASS_PACKAGE_MAPPING.put("BigDecimal", "java.math.BigDecimal");
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
          throw new RuntimeException(String.format("reflector: unable to find matching method %s in class %s",
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
        throw new RuntimeException(String.format("reflector: unable to find matching constructor for class %s",
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
    if (value == null) {
      return null;
    }
    Class<?> clazz = value.getClass();
    if (clazz == Integer.class || clazz == Short.class || clazz == Byte.class) {
      return ((Number)value).longValue();
    } else if (clazz == Float.class) {
      return ((Number)value).doubleValue();
    }
    return value;
  }

  public Class getClazz(String name) {
    Class clazz = _getClass(name);
    if (clazz == null) {
      throw new RuntimeException("reflector: class not found: " + name);
    }
    return clazz;
  }

  Class _getClass(String name) {
    if (name.indexOf('.') == -1) {
      name = CLASS_PACKAGE_MAPPING.getOrDefault(name, "java.lang." + name);
    }
    try {
      return Class.forName(name);
    } catch (ClassNotFoundException e) {
      return null;
    }
  }

  public Object newInstance(String clazz, Object... args) {
    Class c = getClazz(clazz);
    Class[] argTypes = new Class[args.length];
    for (int i = 0; i < args.length; i++) {
      argTypes[i] = unboxIfPossible(args[i].getClass());
    }
    try {
      return getConstructor(c, args, argTypes).newInstance(args);
    } catch (InstantiationException | InvocationTargetException e) {
      throw new RuntimeException(e.getMessage());
    } catch (IllegalAccessException e) {
      throw new RuntimeException(String.format("reflector: unable to access constructor for class %s", clazz));
    }
  }

  /* Java Interop: static fields */
  public Object evalJavaStaticField(String s) {
    if (s.indexOf('/') > -1) {
      String[] classAndField = s.split("/");
      if (classAndField.length < 2) {
        throw new IllegalSyntaxException("reflector: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)");
      }
      String className = classAndField[0];
      String field = classAndField[1];
      Class c = getClazz(className);
      try {
        return ReflectorResult.maybeWrap(c.getField(field).get(c));
      } catch (NoSuchFieldException e) {
        throw new RuntimeException(String.format("reflector: unable to find static field %s in class %s", field, className), e);
      } catch (IllegalAccessException e) {
        throw new RuntimeException(String.format("reflector: unable to access static field %s in class %s", field, className));
      }
    }
    throw new UndefinedIdentifierException(s);
  }

  public Object evalJavaMethod(String method, Object[] args) {
    Object result;
    if (method.startsWith(".-")) {
      if (args.length == 0) {
        throw new IllegalSyntaxException("reflector: malformed member expression, expecting (.member target ...)");
      }
      Object instance = args[0];
      result = evalJavaInstanceField(method, instance);
    } else if (method.indexOf('.') == 0) {
      if (args.length == 0) {
        throw new IllegalSyntaxException("reflector: malformed member expression, expecting (.member target ...)");
      }
      Object instance = args[0];
      args = Arrays.copyOfRange(args, 1, args.length);
      result = evalJavaInstanceMethod(method, instance, args);
    } else if (method.indexOf('/') != -1) {
      result = evalJavaStaticMethod(method, args);
    } else {
      throw new UndefinedIdentifierException(method);
    }
    return ReflectorResult.maybeWrap(result);
  }

  /* Java Interop: instance method call */
  private Object evalJavaInstanceMethod(String m, Object instance, Object[] args) {
    String methodName = m.substring(1);
    Class<?> clazz = instance.getClass();
    boolean isClass = false;
    if (clazz == Symbol.class) {
      clazz = getClazz(instance.toString());
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
      throw new RuntimeException(String.format("reflector: unable to access method %s of %s", methodName, instance));
    } catch (InvocationTargetException e) {
      throw new RuntimeException("reflector: reflection exception");
    }
  }

  /* Java Interop: instance field */
  private Object evalJavaInstanceField(String f, Object instance) {
    Class<?> clazz = instance.getClass();
    boolean isClass = false;
    if (clazz == Symbol.class) {
      clazz = getClazz(instance.toString());
      isClass = true;
    } else {
      clazz = instance.getClass();
    }
    Class c = isClass ? Class.class : clazz;
    String fieldName = f.substring(2);
    try {
      Field field = c.getField(fieldName);
      return upcastIfPossible(field.get(isClass ? clazz : instance));
    } catch (NoSuchFieldException e) {
      throw new RuntimeException(String.format("reflector: unable to find field %s of %s", fieldName, instance));
    } catch (IllegalAccessException e) {
      throw new RuntimeException(String.format("reflector: unable to access method %s of %s", fieldName, instance));
    }
  }

  /* Java Interop: static method call */
  private Object evalJavaStaticMethod(String m, Object[] args) {
    String[] classAndMethod = m.split("/");
    if (classAndMethod.length < 2) {
      throw new IllegalSyntaxException("reflector: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)");
    }
    String className = classAndMethod[0];
    String methodName = classAndMethod[1];
    Class clazz = getClazz(className);
    Class[] argTypes = new Class[args.length];
    for (int i = 0; i < args.length; i++) {
      argTypes[i] = unboxIfPossible(args[i].getClass());
    }
    Method method = getMethod(clazz, methodName, args, argTypes);
    try {
      return upcastIfPossible(method.invoke(null, args));
    } catch (IllegalAccessException e) {
      throw new RuntimeException(String.format("reflector: unable to access static method %s of %s", methodName, clazz.getName()));
    } catch (InvocationTargetException e) {
      throw new RuntimeException("reflector: reflection exception");
    }
  }
}
