package com.iheart.playSwagger

import scala.reflect.runtime.universe._
import ParametricType._

import scala.collection.immutable.SortedMap
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.util.{Failure, Success}

case class ParametricType private(tpe: Type, reifiedTypeName: String, className: String, typeArgsMapping: Map[Line, String]) {
  val resolve: String => String = {
    case ParametricTypeClassName(className, typeArgs) =>
      val resolvedTypes = ParametricType.argStrToList(typeArgs).map(tn => typeArgsMapping.getOrElse(tn, resolve(tn)))
      s"$className[${resolvedTypes.mkString(",")}]"
    case cn => typeArgsMapping.getOrElse(cn, cn)
  }
}

object ParametricType {
  final val ParametricTypeClassName = "^([^\\[\\s,]+)\\[(.+)\\]$".r

  final val DefaultRefinedTypePattern: Regex = raw"eu\.timepit\.refined\.api\.Refined\[(.+)\]".r

  def reifyType(tpe: Type, refinedTypePattern: Regex = DefaultRefinedTypePattern): Type = {
    val dealiasedType = tpe.dealias
    dealiasedType.toString match {
      case refinedTypePattern(_) =>
        dealiasedType.typeArgs.headOption.map(reifyType(_, refinedTypePattern)).getOrElse(dealiasedType)
      case ParametricType.ParametricTypeClassName(_, _) =>
        appliedType(dealiasedType.typeConstructor, dealiasedType.typeArgs.map(reifyType(_, refinedTypePattern)))
      case _ => dealiasedType
    }
  }

  def apply(reifiedTypeName: String)(implicit cl: ClassLoader): ParametricType = {
    val mirror = runtimeMirror(cl)
    reifiedTypeName match {
      case ParametricTypeClassName(className, typeArgsStr) =>
        val sym = mirror.staticClass(className)
        val tpe = sym.selfType
        val typeArgsMapping = SortedMap(tpe.typeArgs.map(_.toString).zip(argStrToList(typeArgsStr)): _*)
        ParametricType(tpe, reifiedTypeName, className, typeArgsMapping)
      case className =>
        val sym = scala.util.Try(mirror.staticClass(className))
          .orElse(scala.util.Try(mirror.staticModule(className))) match {
          case Failure(exception) => throw exception
          case Success(symbol) => symbol
        }
        val tpe = sym.typeSignature
        ParametricType(tpe, className, className, SortedMap.empty)
    }
  }

  def apply[T: TypeTag]: ParametricType = {
    val tpe = implicitly[TypeTag[T]].tpe
    ParametricType(tpe, tpe.typeSymbol.fullName, tpe.typeSymbol.fullName, Map.empty)
  }

  def argStrToList(argsStr: String): List[String] = {
    val args = scala.collection.mutable.ListBuffer.empty[String]
    var depth = 0
    val nextArg = ArrayBuffer.empty[Char]

    for (c <- argsStr) {
      if (!Character.isWhitespace(c) && c != ',') {
        nextArg += c
      }

      if (c == ',') {
        if (depth <= 0) {
          args += String.valueOf(nextArg.toArray)
          nextArg.clear()
        } else {
          nextArg += c
        }
      } else if (c == '[') {
        depth += 1
      } else if (c == ']') {
        depth -= 1
      }
    }

    if (nextArg.nonEmpty) args += String.valueOf(nextArg.toArray)

    args.toList
  }
}
