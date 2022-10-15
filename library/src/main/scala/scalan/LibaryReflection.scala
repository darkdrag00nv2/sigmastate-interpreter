package scalan

import scalan.reflection.ReflectionData.{registerClassEntry, registerClassOnly}
import scalan.reflection.{SRConstructor, SRMethod}
import special.collection.Colls

object LibaryReflection {

  registerClassOnly(classOf[Colls#PairColl[_, _]])

  { val clazz = classOf[scalan.TypeDescs#FuncElem[_, _]]
    registerClassEntry(clazz,
      constructors = Array(
        new SRConstructor[Any](Array(classOf[scalan.Scalan], classOf[scalan.TypeDescs#Elem[_]], classOf[scalan.TypeDescs#Elem[_]])) {
          override def newInstance(args: AnyRef*): Any = {
            val ctx = args(0).asInstanceOf[scalan.Scalan]
            new ctx.FuncElem(args(1).asInstanceOf[ctx.Elem[_]], args(2).asInstanceOf[ctx.Elem[_]])
          }
        }
      )
    )
  }

  { val clazz = classOf[scalan.TypeDescs#PairElem[_, _]]
    registerClassEntry(clazz,
      constructors = Array(
        new SRConstructor[Any](Array(classOf[scalan.Scalan], classOf[scalan.TypeDescs#Elem[_]], classOf[scalan.TypeDescs#Elem[_]])) {
          override def newInstance(args: AnyRef*): Any = {
            val ctx = args(0).asInstanceOf[scalan.Scalan]
            new ctx.PairElem(args(1).asInstanceOf[ctx.Elem[_]], args(2).asInstanceOf[ctx.Elem[_]])
          }
        }
      )
    )
  }

  { val clazz = classOf[scalan.primitives.Thunks#ThunkElem[_]]
    registerClassEntry(clazz,
      constructors = Array(
        new SRConstructor[Any](Array(classOf[scalan.Scalan], classOf[scalan.TypeDescs#Elem[_]])) {
          override def newInstance(args: AnyRef*): Any = {
            val ctx = args(0).asInstanceOf[scalan.Scalan]
            new ctx.ThunkElem(args(1).asInstanceOf[ctx.Elem[_]])
          }
        }
      )
    )
  }

  { val clazz = classOf[Colls#Coll[_]]
    val ctx = null.asInstanceOf[scalan.Library] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]], classOf[scalan.Base#Ref[_]])
        ("indexOf", paramTypes) ->
            new SRMethod(clazz, "indexOf", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.indexOf(args(0).asInstanceOf[ctx.Ref[a]], args(1).asInstanceOf[ctx.Ref[Int]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]])
        ("exists", paramTypes) ->
            new SRMethod(clazz, "exists", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.exists(args(0).asInstanceOf[ctx.Ref[a => Boolean]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array[Class[_]]()
        ("length", paramTypes) ->
            new SRMethod(clazz, "length", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[_] =>
                  obj.length
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]], classOf[scalan.Base#Ref[_]])
        ("getOrElse", paramTypes) ->
            new SRMethod(clazz, "getOrElse", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.getOrElse(args(0).asInstanceOf[ctx.Ref[Int]], args(1).asInstanceOf[ctx.Ref[a]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]], classOf[scalan.Base#Ref[_]])
        ("slice", paramTypes) ->
            new SRMethod(clazz, "slice", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[_] =>
                  obj.slice(args(0).asInstanceOf[ctx.Ref[Int]], args(1).asInstanceOf[ctx.Ref[Int]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]], classOf[scalan.Base#Ref[_]])
        ("foldLeft", paramTypes) ->
            new SRMethod(clazz, "foldLeft", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.foldLeft(args(0).asInstanceOf[ctx.Ref[Any]], args(1).asInstanceOf[ctx.Ref[((Any, a)) => Any]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]])
        ("flatMap", paramTypes) ->
            new SRMethod(clazz, "flatMap", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.flatMap(args(0).asInstanceOf[ctx.Ref[a => ctx.Coll[Any]]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]])
        ("append", paramTypes) ->
            new SRMethod(clazz, "append", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.append(args(0).asInstanceOf[ctx.Ref[ctx.Coll[a]]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]])
        ("map", paramTypes) ->
            new SRMethod(clazz, "map", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.map(args(0).asInstanceOf[ctx.Ref[a => Any]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]])
        ("filter", paramTypes) ->
            new SRMethod(clazz, "filter", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.filter(args(0).asInstanceOf[ctx.Ref[a => Boolean]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]], classOf[scalan.Base#Ref[_]], classOf[scalan.Base#Ref[_]])
        ("patch", paramTypes) ->
            new SRMethod(clazz, "patch", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.patch(args(0).asInstanceOf[ctx.Ref[Int]],
                    args(1).asInstanceOf[ctx.Ref[ctx.Coll[a]]],
                    args(2).asInstanceOf[ctx.Ref[Int]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]], classOf[scalan.Base#Ref[_]])
        ("updateMany", paramTypes) ->
            new SRMethod(clazz, "updateMany", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.updateMany(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Int]]],
                    args(1).asInstanceOf[ctx.Ref[ctx.Coll[a]]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]], classOf[scalan.Base#Ref[_]])
        ("updated", paramTypes) ->
            new SRMethod(clazz, "updated", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.updated(args(0).asInstanceOf[ctx.Ref[Int]], args(1).asInstanceOf[ctx.Ref[a]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]])
        ("forall", paramTypes) ->
            new SRMethod(clazz, "forall", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.forall(args(0).asInstanceOf[ctx.Ref[a => Boolean]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array[Class[_]]()
        ("indices", paramTypes) ->
            new SRMethod(clazz, "indices", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[_] =>
                  obj.indices
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]])
        ("apply", paramTypes) ->
            new SRMethod(clazz, "apply", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[_] =>
                  obj.apply(args(0).asInstanceOf[ctx.Ref[Int]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]])
        ("zip", paramTypes) ->
            new SRMethod(clazz, "zip", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.Coll[a] =>
                  obj.zip(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Any]]])
              }
            }
      }
      )
    )
  }

  { val clazz = classOf[Colls#CollBuilder]
    val ctx = null.asInstanceOf[scalan.Library] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scalan.Base#Ref[_]], classOf[scalan.Base#Ref[_]])
        ("xor", paramTypes) ->
            new SRMethod(clazz, "xor", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.CollBuilder =>
                  obj.xor(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]], args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
              }
            }
      },
      {
        val paramTypes: Seq[Class[_]] = Array(classOf[scala.collection.Seq[_]], classOf[scalan.TypeDescs#Elem[_]])
        ("fromItems", paramTypes) ->
            new SRMethod(clazz, "fromItems", paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: ctx.CollBuilder =>
                  obj.fromItems(
                    args(0).asInstanceOf[Seq[ctx.Ref[Any]]]: _*
                  )(args(1).asInstanceOf[ctx.Elem[Any]])
              }
            }
      }
      )
    )
  }

  { val ctx = null.asInstanceOf[scalan.Library] // ok! type level only
    val clazz = classOf[ctx.Coll.CollElem[_, _]]
    registerClassEntry(clazz,
      constructors = Array(
        new SRConstructor[Any](Array(clazz.getDeclaringClass, classOf[TypeDescs#Elem[_]])) {
          override def newInstance(args: AnyRef*): Any = {
            val cake = args(0).asInstanceOf[ctx.Coll.type]
            new cake.CollElem()(args(1).asInstanceOf[ctx.Elem[_]])
          }
        }
      )
    )
  }
}