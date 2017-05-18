package core.evaluator

import core.exceptions.IllegalSyntaxException
import core.exceptions.UndefinedIdentifierException

import java.lang.reflect.Constructor
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.util.Arrays

class Reflector {

    private fun getMethod(clazz: Class<*>, name: String, args: Array<Any>, parameterTypes: Array<Class<*>?>): Method {
        try {
            return clazz.getMethod(name, *parameterTypes)
        } catch (e: NoSuchMethodException) {
            // no exact match found, try to find inexact match
            // FIXME Workaround: save state before downcasting
            val argsOld = Arrays.copyOf(args, args.size)
            val paramsOld = Arrays.copyOf(parameterTypes, parameterTypes.size)
            downcastArgs(args, parameterTypes)
            try {
                return clazz.getMethod(name, *parameterTypes)
            } catch (ex: NoSuchMethodException) {
                try {
                    // FIXME Workaround: restore previous state
                    // restore saved state
                    System.arraycopy(argsOld, 0, args, 0, argsOld.size)
                    System.arraycopy(paramsOld, 0, parameterTypes, 0, paramsOld.size)
                    castToObject(parameterTypes)
                    return clazz.getMethod(name, *parameterTypes)
                } catch (ex2: NoSuchMethodException) {
                    throw RuntimeException(String.format("reflector: unable to find matching method %s in class %s",
                            name, clazz.name))
                }
            }
        }
    }

    private fun getConstructor(clazz: Class<*>, args: Array<Any>, parameterTypes: Array<Class<*>?>): Constructor<*> {
        try {
            return clazz.getConstructor(*parameterTypes)
        } catch (e: NoSuchMethodException) {
            // no exact match found, try to find inexact match
            downcastArgs(args, parameterTypes)
            try {
                return clazz.getConstructor(*parameterTypes)
            } catch (ex: NoSuchMethodException) {
                throw RuntimeException(String.format("reflector: unable to find matching constructor for class %s",
                        clazz.name))
            }
        }
    }

    private fun downcastArgs(args: Array<Any>, parameterTypes: Array<Class<*>?>) {
        for (i in parameterTypes.indices) {
            val parameterType = parameterTypes[i]
            if (Number::class.java.isAssignableFrom((BOXED as Map<Class<*>?, Class<*>?>).getOrDefault(parameterType, parameterType))) {
                // cast to int
                parameterTypes[i] = Int::class.java
                args[i] = (args[i] as Number).toInt()
            }
        }
    }

    private fun castToObject(parameterTypes: Array<Class<*>?>) {
        for (i in parameterTypes.indices) {
            parameterTypes[i] = Any::class.java
        }
    }

    private fun unboxIfPossible(clazz: Class<*>): Class<*> {
        return (UNBOXED as Map<Class<*>, Class<*>>).getOrDefault(clazz, clazz)
    }

    fun getClazz(name: String): Class<*> {
        val clazz = _getClass(name) ?: throw RuntimeException("reflector: class not found: " + name)
        return clazz
    }

    fun _getClass(name: String): Class<*>? {
        try {
            when {
                name.indexOf('.') == -1 -> return Class.forName((CLASS_PACKAGE_MAPPING as Map<String, String>).getOrDefault(name, "java.lang." + name))
                else -> return Class.forName(name)
            }
        } catch (e: ClassNotFoundException) {
            return null
        }
    }

    fun newInstance(clazz: String, args: Array<Any>): Any {
        val c = getClazz(clazz)
        val argTypes = arrayOfNulls<Class<*>>(args.size)
        for (i in args.indices) {
            argTypes[i] = unboxIfPossible(args[i].javaClass)
        }
        try {
            return getConstructor(c, args, argTypes).newInstance(*args)
        } catch (e: InstantiationException) {
            throw RuntimeException(e.message)
        } catch (e: InvocationTargetException) {
            throw RuntimeException(e.message)
        } catch (e: IllegalAccessException) {
            throw RuntimeException(String.format("reflector: unable to access constructor for class %s", clazz))
        }
    }

    /* Java Interop: static fields */
    fun evalJavaStaticField(s: String): Any {
        if (s.indexOf('/') > -1) {
            val classAndField = s.split("/".toRegex()).dropLastWhile(String::isEmpty).toTypedArray()
            if (classAndField.size < 2) {
                throw IllegalSyntaxException("reflector: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)")
            }
            val className = classAndField[0]
            val fieldName = classAndField[1]
            val c = getClazz(className)
            try {
                val field = c.getField(fieldName)
                if (!Modifier.isStatic(field.modifiers)) {
                    throw RuntimeException(
                            String.format("reflector: unable to find static field %s of %s", fieldName, className))
                }
                return field.get(c)
            } catch (e: NoSuchFieldException) {
                throw RuntimeException(String.format("reflector: unable to find static field %s in class %s", fieldName, className), e)
            } catch (e: IllegalAccessException) {
                throw RuntimeException(String.format("reflector: unable to access static field %s in class %s", fieldName, className))
            }
        }
        throw UndefinedIdentifierException(s)
    }

    fun evalJavaMethod(method: String, args: Array<Any>): Any {
        val result: Any
        if (method.startsWith(".-")) {
            if (args.isEmpty()) {
                throw IllegalSyntaxException("reflector: malformed member expression, expecting (.member target ...)")
            }
            val instance = args[0]
            result = evalJavaInstanceField(method, instance)
        } else if (method.indexOf('.') == 0) {
            if (args.isEmpty()) {
                throw IllegalSyntaxException("reflector: malformed member expression, expecting (.member target ...)")
            }
            val instance = args[0]
            val rest = Arrays.copyOfRange(args, 1, args.size)
            result = evalJavaInstanceMethod(method, instance, rest)
        } else if (method.indexOf('/') != -1) {
            result = evalJavaStaticMethod(method, args)
        } else {
            throw UndefinedIdentifierException(method)
        }
        return result
    }

    /* Java Interop: instance method call */
    private fun evalJavaInstanceMethod(m: String, instance: Any, args: Array<Any>): Any {
        val methodName = m.substring(1)
        val clazz = instance.javaClass
        val argTypes = arrayOfNulls<Class<*>>(args.size)
        for (i in args.indices) {
            argTypes[i] = unboxIfPossible(args[i].javaClass)
        }
        val method = getMethod(clazz, methodName, args, argTypes)
        try {
            return method.invoke(instance, *args)
        } catch (e: IllegalAccessException) {
            throw RuntimeException(String.format("reflector: unable to access method %s of %s", methodName, instance))
        } catch (e: InvocationTargetException) {
            throw RuntimeException("reflector: reflection exception")
        }
    }

    /* Java Interop: instance field */
    private fun evalJavaInstanceField(f: String, instance: Any): Any {
        val clazz = instance.javaClass
        val fieldName = f.substring(2)
        try {
            val field = clazz.getField(fieldName)
            return field.get(instance)
        } catch (e: NoSuchFieldException) {
            throw RuntimeException(String.format("reflector: unable to find field %s of %s", fieldName, instance))
        } catch (e: IllegalAccessException) {
            throw RuntimeException(String.format("reflector: unable to access method %s of %s", fieldName, instance))
        }
    }

    /* Java Interop: static method call */
    private fun evalJavaStaticMethod(m: String, args: Array<Any>): Any {
        val classAndMethod = m.split("/".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray()
        if (classAndMethod.size < 2) {
            throw IllegalSyntaxException("reflector: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)")
        }
        val className = classAndMethod[0]
        val methodName = classAndMethod[1]
        val clazz = getClazz(className)
        val argTypes = arrayOfNulls<Class<*>>(args.size)
        for (i in args.indices) {
            argTypes[i] = unboxIfPossible(args[i].javaClass)
        }
        val method = getMethod(clazz, methodName, args, argTypes)
        if (!Modifier.isStatic(method.modifiers)) {
            throw RuntimeException(String.format("reflector: unable to find static method %s of %s", methodName, clazz.name))
        }
        try {
            return method.invoke(null, *args)
        } catch (e: IllegalAccessException) {
            throw RuntimeException(String.format("reflector: unable to access static method %s of %s", methodName, clazz.name))
        } catch (e: InvocationTargetException) {
            throw RuntimeException("reflector: reflection exception")
        }
    }

    companion object {

        private val UNBOXED = HashMap<Class<*>, Class<*>>()

        init {
            UNBOXED.put(Byte::class.java,    Byte::class.java)
            UNBOXED.put(Short::class.java,   Short::class.java)
            UNBOXED.put(Int::class.java,     Int::class.java)
            UNBOXED.put(Long::class.java,    Long::class.java)
            UNBOXED.put(Float::class.java,   Float::class.java)
            UNBOXED.put(Double::class.java,  Double::class.java)
            UNBOXED.put(Char::class.java,    Char::class.java)
            UNBOXED.put(Boolean::class.java, Boolean::class.java)
        }

        /* Some common classes that are not in java.lang. package could be resolved without package name */
        private val CLASS_PACKAGE_MAPPING = HashMap<String, String>()

        init {
            CLASS_PACKAGE_MAPPING.put("BigInteger", "java.math.BigInteger")
            CLASS_PACKAGE_MAPPING.put("BigDecimal", "java.math.BigDecimal")
        }

        private val BOXED = HashMap<Class<*>, Class<*>>()

        init {
            for ((key, value) in UNBOXED) {
                BOXED.put(value, key)
            }
        }
    }
}
