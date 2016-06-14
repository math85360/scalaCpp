package org.scalacpp.cppast

case class CppField(override val mods: CppModifier, name: String, tpe: CppType, default: Option[CppExpr]) extends CppDef {
  override def toCpp(cls: CppClass) = if (default.isEmpty) "" else s""

  override def toH(cls: CppClass) = s"${mods.staticModifier} ${tpe.tpe} $name;"
}