package org.scalacpp.cppast

import scala.tools.nsc.Global

case class CppType(tpe: String)

object CppType {
  var map = Map[Object, String]()

  def buildMap(implicit global: Global) {
    if (map.isEmpty) {
      import global._
      map = map ++ Map(typeOf[Unit] -> "void",
        typeOf[Int] -> "int",
        typeOf[Long] -> "long",
        typeOf[Boolean] -> "bool",
        typeOf[String] -> "string",
        typeOf[Double] -> "double")
    }
  }

  def fromScala(s: Any)(implicit global: Global): CppType = {
    import global._
    buildMap
    new CppType(s match {
      case t: Tree =>
        System.err.println(t.tpe + " " + t.symbol.tpe)
        map
          .collectFirst({ case (k, v) if k == t.symbol.tpe => v })
          .getOrElse(t.symbol.simpleName.decoded)
    })
  }
}