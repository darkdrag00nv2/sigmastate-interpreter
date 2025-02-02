package scalan.util

import scala.language.existentials

object ReflectionUtil {

  implicit class ClassOps(val cl: Class[_]) extends AnyVal {
    private def isSpecialChar(c: Char): Boolean = {
      ('0' <= c && c <= '9') || c == '$'
    }
    def safeSimpleName: String = {
      if (cl.getEnclosingClass == null) return cl.getSimpleName
      val simpleName = cl.getName.substring(cl.getEnclosingClass.getName.length)
      val length = simpleName.length
      var index = 0
      while (index < length && isSpecialChar(simpleName.charAt(index))) { index += 1 }
      // Eventually, this is the empty string iff this is an anonymous class
      simpleName.substring(index)
    }
  }
}
