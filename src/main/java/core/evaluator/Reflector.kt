package core.evaluator

import core.exceptions.IllegalSyntaxException
import core.exceptions.UndefinedIdentifierException
import java.lang.reflect.Constructor
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.math.BigDecimal
import java.math.BigInteger
import kotlin.text.isEmpty

class Reflector {

    companion object {

        private val UNBOXED = hashMapOf(
                Byte::class.javaObjectType    to Byte::class.java,
                Short::class.javaObjectType   to Short::class.java,
                Int::class.javaObjectType     to Int::class.java,
                Long::class.javaObjectType    to Long::class.java,
                Float::class.javaObjectType   to Float::class.java,
                Double::class.javaObjectType  to Double::class.java,
                Char::class.javaObjectType    to Char::class.java,
                Boolean::class.javaObjectType to Boolean::class.java
        )

        /* Some common classes that are not in java.lang. package could be resolved without package name */
        private val CLASS_PACKAGE_MAPPING = HashMap<String, String>().apply {
            arrayOf(BigInteger::class.java, BigDecimal::class.java).forEach { put(it.simpleName, it.name) }
        }

        private val BOXED = HashMap<Class<*>?, Class<*>?>().apply {
            UNBOXED.forEach { key, value -> put(value, key) }
        }
    }

    private fun getMethod(clazz: Class<*>?, name: String, args: Array<out Any?>, parameterTypes: Array<Class<*>?>): Method {
        clazz!!
        try {
            return clazz.getMethod(name, *parameterTypes)
        } catch (e: NoSuchMethodException) {
            // no exact match found, try to find inexact match
            // FIXME Workaround: save state before downcasting
            val argsOld = args.copyOf()
            val paramsOld = parameterTypes.copyOf()
            downcastArgs(args as Array<Any?>, parameterTypes)
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
                    throw RuntimeException("reflector: unable to find matching method $name in class ${clazz.name}")
                }
            }
        }
    }

    private fun getConstructor(clazz: Class<*>, args: Array<Any?>, parameterTypes: Array<Class<*>?>): Constructor<*> {
        return try {
            clazz.getConstructor(*parameterTypes)
        } catch (e: NoSuchMethodException) {
            // no exact match found, try to find inexact match
            downcastArgs(args, parameterTypes)
            try {
                clazz.getConstructor(*parameterTypes)
            } catch (ex: NoSuchMethodException) {
                throw RuntimeException("reflector: unable to find matching constructor for class ${clazz.name}")
            }
        }
    }

    private fun downcastArgs(args: Array<Any?>, parameterTypes: Array<Class<*>?>) {
        for (i in parameterTypes.indices) {
            parameterTypes[i]?.let {
                if (Number::class.java.isAssignableFrom(BOXED.getOrDefault(it, it))) {
                    // cast to int
                    parameterTypes[i] = Int::class.java
                    args[i] = (args[i] as Number).toInt()
                }
            }
        }
    }

    private fun castToObject(parameterTypes: Array<Class<*>?>) {
        for (i in parameterTypes.indices) {
            parameterTypes[i] = Object::class.java
        }
    }

    private fun unboxIfPossible(clazz: Class<*>) = UNBOXED.getOrDefault(clazz, clazz)

    fun getClazz(name: String) = getClazzOrNull(name) ?: throw RuntimeException("reflector: class not found: $name")

    fun getClazzOrNull(name: String): Class<*>? = try {
        when {
            name.contains('.') -> Class.forName(name)
            else -> Class.forName(CLASS_PACKAGE_MAPPING.getOrDefault(name, "java.lang.$name"))
        }
    } catch (e: ClassNotFoundException) {
        null
    }

    fun newInstance(clazz: String, args: Array<Any?>): Any {
        val c = getClazz(clazz)
        val argTypes = arrayOfNulls<Class<*>>(args.size)
        for (i in args.indices) {
            argTypes[i] = unboxIfPossible(args[i]!!.javaClass)
        }
        try {
            return getConstructor(c, args, argTypes).newInstance(*args)
        } catch (e: InstantiationException) {
            throw RuntimeException(e.message)
        } catch (e: InvocationTargetException) {
            throw RuntimeException(e.message)
        } catch (e: IllegalAccessException) {
            throw RuntimeException("reflector: unable to access constructor for class $clazz")
        }
    }

    /* Java Interop: static fields */
    fun evalJavaStaticField(s: String): Any? {
        if (s.contains('/')) {
            val classAndField = s.split('/').dropLastWhile(String::isEmpty)
            if (classAndField.size < 2) {
                throw IllegalSyntaxException("reflector: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)")
            }
            val (className, fieldName) = classAndField
            val c = getClazz(className)
            try {
                val field = c.getField(fieldName)
                if (!Modifier.isStatic(field.modifiers)) {
                    throw RuntimeException("reflector: unable to find static field $fieldName of $className")
                }
                return field.get(c)
            } catch (e: NoSuchFieldException) {
                throw RuntimeException("reflector: unable to find static field $fieldName in class $className", e)
            } catch (e: IllegalAccessException) {
                throw RuntimeException("reflector: unable to access static field $fieldName in class $className")
            }
        }
        throw UndefinedIdentifierException(s)
    }

    fun evalJavaMethod(method: String, args: Array<out Any?>): Any? {
        val result: Any?
        if (method.startsWith(".-")) {
            if (args.isEmpty()) {
                throw IllegalSyntaxException("reflector: malformed member expression, expecting (.member target ...)")
            }
            val instance = args[0]
            result = evalJavaInstanceField(method, instance)
        } else if (method.startsWith('.')) {
            if (args.isEmpty()) {
                throw IllegalSyntaxException("reflector: malformed member expression, expecting (.member target ...)")
            }
            val instance = args[0]
            val rest = args.copyOfRange(1, args.size)
            result = evalJavaInstanceMethod(method, instance, rest)
        } else if (method.contains('/')) {
            result = evalJavaStaticMethod(method, args)
        } else {
            throw UndefinedIdentifierException(method)
        }
        return result
    }

    /* Java Interop: instance method call: (.toString (new Object)) */
    private fun evalJavaInstanceMethod(methodName: String, instance: Any?, args: Array<out Any?>): Any? {
        val argTypes = arrayOfNulls<Class<*>>(args.size).apply {
            for (i in args.indices) {
                set(i, args[i]?.let { unboxIfPossible(it.javaClass) })
            }
        }
        /* Remove leading . char */
        return methodName.substring(1).let {
            try {
                getMethod(instance?.javaClass, it, args, argTypes).invoke(instance, *args)
            } catch (e: IllegalAccessException) {
                throw RuntimeException("reflector: unable to access method $it of $instance")
            } catch (e: InvocationTargetException) {
                throw RuntimeException("reflector: reflection exception")
            }
        }
    }

    /* Java Interop: instance field: (.-x (new java.awt.Point 15 4)) */
    private fun evalJavaInstanceField(field: String, instance: Any?) = field.substring(2).let {
        try {
            instance?.javaClass?.getField(it)?.get(instance)
        } catch (e: NoSuchFieldException) {
            throw RuntimeException("reflector: unable to find field $it of $instance")
        } catch (e: IllegalAccessException) {
            throw RuntimeException("reflector: unable to access method $it of $instance")
        }
    }

    /* Java Interop: static method call */
    private fun evalJavaStaticMethod(m: String, args: Array<out Any?>): Any? {
        val classAndMethod = m.split('/').dropLastWhile { it.isEmpty() }
        if (classAndMethod.size < 2) {
            throw IllegalSyntaxException("reflector: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)")
        }
        val (className, methodName) = classAndMethod
        val clazz = getClazz(className)
        val argTypes = arrayOfNulls<Class<*>>(args.size)
        for (i in args.indices) {
            argTypes[i] = unboxIfPossible(args[i]!!.javaClass)
        }
        val method = getMethod(clazz, methodName, args, argTypes)
        if (!Modifier.isStatic(method.modifiers)) {
            throw RuntimeException("reflector: unable to find static method $methodName of ${clazz.name}")
        }
        return try {
            method(null, *args)
        } catch (e: IllegalAccessException) {
            throw RuntimeException("reflector: unable to access static method $methodName of ${clazz.name}")
        } catch (e: InvocationTargetException) {
            throw RuntimeException("reflector: reflection exception")
        }
    }
}
