package core.evaluator;

import core.exceptions.IllegalSyntaxException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

class Reflector {

  // TODO Overloaded method resolution
  // TODO Native methods? (.getClass)
  private Method getMethod(Class clazz, String name, Class<?>... parameterTypes) {
    try {
      return clazz.getMethod(name, parameterTypes);
    } catch (NoSuchMethodException e) {
      throw new IllegalSyntaxException(String.format("method %s not found in class %s", name, clazz.getName()));
    }
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
      if (Character.isUpperCase(name.substring(name.lastIndexOf('.') + 1).charAt(0))) {
        clazz = getClass(name);
        isClass = true;
      } else {
        clazz = o.getClass();
      }
      Object[] args = sexp.subList(2, sexp.size()).toArray();
      Class[] argTypes = new Class[args.length];
      for (int i = 0; i < args.length; i++) {
        argTypes[i] = args[i].getClass();
      }
      Method method = getMethod(isClass ? Class.class : clazz, methodName, argTypes);
      try {
        return method.invoke(isClass ? clazz : o, args);
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
        argTypes[i] = args[i].getClass();
      }
      Method method = getMethod(clazz, methodName, argTypes);
      try {
        return method.invoke(null, args);
      } catch (IllegalAccessException e) {
        throw new IllegalSyntaxException(String.format("unable to access static method %s of %s", methodName, clazz.getName()));
      } catch (InvocationTargetException e) {
        e.printStackTrace();
      }
    }
    throw new IllegalArgumentException("undefined identifier: " + op);
  }
}
