package org.scalacpp.cppast

case class CppFunction(override val mods: CppModifier, name: String, tpe: CppType, params: List[CppField]) extends CppDef {

  override def toCpp(cls: CppClass) = params
    .map({ case CppField(fmods, fname, ftpe, fdefault) => s"${ftpe.tpe} $fname" })
    .mkString(s"${tpe.tpe} ${cls.name}::$name(", ", ", ") {\n}")

  override def toH(cls: CppClass) = params
    .map({ case CppField(fmods, fname, ftpe, fdefault) => s"$ftpe $fname" })
    .mkString(s"${mods.staticModifier} ${tpe.tpe} $name(", ", ", ");")
}