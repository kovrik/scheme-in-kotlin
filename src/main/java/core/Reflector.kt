package core

import core.exceptions.IllegalSyntaxException
import core.exceptions.UndefinedIdentifierException
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Modifier
import java.math.BigDecimal
import java.math.BigInteger
import kotlin.text.isEmpty

class Reflector {

    companion object {
        private val CLASSES = listOf(Boolean::class, Byte::class, Char::class, Short::class, Int::class, Long::class, Float::class, Double::class)
        private val BOXED = CLASSES.associateBy({ it.java }, { it.javaObjectType })
        private val UNBOXED = CLASSES.associateBy({ it.javaObjectType }, { it.java })

        /* Some common classes are not in java.lang. package, but we don't want to use fully qualified name to use them */
        private val CLASS_PACKAGE_MAPPING = arrayOf(BigInteger::class.java, BigDecimal::class.java).associateBy({ it.simpleName }, { it.name })
    }

    private val name: String = "reflector"

    fun getClazz(className: String) = getClazzOrNull(className) ?: throw ClassNotFoundException("$name: class not found: $className")

    fun getClazzOrNull(name: String): Class<*>? = try {
        when {
            '.' in name -> Class.forName(name)
            else -> Class.forName(CLASS_PACKAGE_MAPPING.getOrDefault(name, "java.lang.$name"))
        }
    } catch (e: ClassNotFoundException) {
        null
    }

    fun newInstance(className: String, args: Array<out Any?>): Any {
        val c = getClazz(className)
        val argTypes = arrayOfNulls<Class<*>>(args.size).apply {
            args.forEachIndexed { index, arg -> set(index, unboxIfPossible(arg)) }
        }
        try {
            return try {
                c.getConstructor(*argTypes).apply { isAccessible = true }.newInstance(*args)
            } catch (e: NoSuchMethodException) {
                // no exact match found, try to find inexact match
                val (newArgs, newTypes) = downcastArgs(args, argTypes)
                c.getConstructor(*newTypes).apply { isAccessible = true }.newInstance(*newArgs)
            }
        } catch (ex: NoSuchMethodException) {
            throw NoSuchMethodException("$name: unable to find matching constructor for class ${c.name}")
        } catch (e: IllegalAccessException) {
            throw IllegalAccessException("$name: unable to access constructor for class $className")
        }
    }

    /* Java Interop: static fields */
    fun evalJavaStaticField(className: String, fieldName: String): Any? = try {
        val c = getClazz(className)
        c.getField(fieldName).apply {
            isAccessible = true
            if (!Modifier.isStatic(modifiers)) {
                throw NoSuchFieldException("$name: unable to find static field $fieldName of $className")
            }
        }.get(c)
    } catch (e: NoSuchFieldException) {
        throw NoSuchFieldException("$name: unable to find static field $fieldName in class $className")
    } catch (e: IllegalAccessException) {
        throw IllegalAccessException("$name: unable to access static field $fieldName in class $className")
    }

    fun evalJavaMethod(methodString: String, args: Array<out Any?>) = when {
        '/' in methodString -> methodString.split('/').filterNot(String::isEmpty).let {
            when (it.size > 1) {
                true  -> evalJavaStaticMethod(it[0], it[1], args)
                false -> throw IllegalSyntaxException("$name: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)")
            }
        }
        args.isEmpty() -> throw IllegalSyntaxException("$name: malformed member expression, expecting (.member target ...)")
        methodString.startsWith(".-") -> evalJavaInstanceField(methodString.drop(2), instance = args[0]!!)
        methodString.startsWith('.')  -> evalJavaInstanceMethod(methodString.drop(1), instance = args[0]!!, args = args.copyOfRange(1, args.size))
        else -> throw UndefinedIdentifierException(methodString)
    }

    /* Returns Pair(method: Method, args: Array<out Any?>) */
    private fun getMethodAndArgs(clazz: Class<*>, methodName: String, args: Array<out Any?>, types: Array<Class<*>?>) = try {
        Pair(clazz.getMethod(methodName, *types), args)
    } catch (e: NoSuchMethodException) {
        // no exact match found, try to find inexact match
        val (newArgs, newTypes) = downcastArgs(args, types)
        try {
            Pair(clazz.getMethod(methodName, *newTypes), newArgs)
        } catch (ex: NoSuchMethodException) {
            try {
                Pair(clazz.getMethod(methodName, *Array(types.size) { Object::class.java }), args)
            } catch (ex2: NoSuchMethodException) {
                throw NoSuchMethodException("$name: unable to find matching method $methodName in class ${clazz.name}")
            }
        }
    }

    private fun downcastArgs(args: Array<out Any?>, types: Array<out Class<*>?>): Pair<Array<out Any?>, Array<out Class<*>?>> {
        val newArgs = arrayOfNulls<Any?>(args.size)
        val newTypes = arrayOfNulls<Class<*>?>(types.size)
        for (i in types.indices) {
            types[i]?.let {
                if (Number::class.java.isAssignableFrom(BOXED.getOrDefault(it, it))) {
                    // downcast to int
                    newTypes[i] = Int::class.java
                    newArgs[i] = (args[i] as Number).toInt()
                } else {
                    newTypes[i] = types[i]
                    newArgs[i] = args[i]
                }
            }
        }
        return newArgs to newTypes
    }

    private fun unboxIfPossible(it: Any?) = it?.let { UNBOXED.getOrDefault(it.javaClass, it.javaClass) }

    /* Java Interop: instance method call: (.toString (new Object)) */
    fun evalJavaInstanceMethod(methodName: String, instance: Any, args: Array<out Any?>): Any? = methodName.let {
        try {
            val argTypes = arrayOfNulls<Class<*>>(args.size).apply {
                args.forEachIndexed { index, arg -> set(index, unboxIfPossible(arg)) }
            }
            val (method, methodArgs) = getMethodAndArgs(instance.javaClass, it, args, argTypes)
            method.apply { isAccessible = true }(instance, *methodArgs)
        } catch (e: IllegalAccessException) {
            throw IllegalAccessException("$name: unable to access method $it of ${instance.javaClass.name}")
        } catch (e: InvocationTargetException) {
            when (e.cause) {
                null -> throw RuntimeException("$name: invocation target exception")
                else -> throw e.cause as Throwable
            }
        }
    }

    /* Java Interop: instance field: (.-x (new java.awt.Point 15 4)) */
    private fun evalJavaInstanceField(fieldName: String, instance: Any) = fieldName.let {
        try {
            instance.javaClass.getField(it).apply { isAccessible = true }.get(instance)
        } catch (e: IllegalAccessException) {
            throw IllegalAccessException("$name: unable to access method $it of ${instance.javaClass.name}")
        } catch (e: NoSuchFieldException) {
            throw NoSuchFieldException("$name: unable to find field $it of ${instance.javaClass.name}")
        }
    }

    /* Java Interop: static method call */
    fun evalJavaStaticMethod(className: String, methodName: String, args: Array<out Any?>): Any? {
        val clazz = getClazz(className)
        val argTypes = arrayOfNulls<Class<*>>(args.size).apply {
            args.forEachIndexed { index, arg -> set(index, unboxIfPossible(arg)) }
        }
        val (method, methodArgs) = getMethodAndArgs(clazz, methodName, args, argTypes)
        if (!Modifier.isStatic(method.modifiers)) {
            throw RuntimeException("$name: unable to find static method $methodName of ${clazz.name}")
        }
        return try {
            method.apply { isAccessible = true }(null, *methodArgs)
        } catch (e: IllegalAccessException) {
            throw IllegalAccessException("$name: unable to access static method $methodName of ${clazz.name}")
        } catch (e: InvocationTargetException) {
            when (e.cause) {
                null -> throw RuntimeException("$name: invocation target exception")
                else -> throw e.cause as Throwable
            }
        }
    }
}
