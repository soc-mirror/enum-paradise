package scalax

import scala.reflect.macros.Context

object EnumMacroUsingCompanionObject {
  def apply(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    val EnumValue = 1L << 48
    val ENUM = EnumValue.asInstanceOf[FlagSet]
    val StaticValue = 1L << 23
    val STATIC = StaticValue.asInstanceOf[FlagSet]

    val List(Expr(classDef @ ClassDef(_, className, _, template))) = annottees
    val Template(parents, self, body) = template

    case class EnumDef(ordinal: Int, name: String, tree: Tree)

    def enumInstance(ordinal: Int, name: String): EnumDef =
      EnumDef(ordinal, name, q"new $className($name, $ordinal)")

    lazy val enumDefs: List[EnumDef] = body.zipWithIndex.toList.collect {
      // <ENUM>
      case (Ident(termName: TermName), index) => enumInstance(index, termName.encoded)
      // <ENUM>(<enumParam>, ...)
      // <ENUM> { <enumDef>, ... }
      // <ENUM>(<enumParam>, ...) { <enumDef>, ... }
    }

    def enumIdent(enumDef: EnumDef) = Select(Ident(className.toTermName), newTermName(enumDef.name))

    // <ENUM> ===> val <ENUM>: <EnumClass> = new <EnumClass>(name = "<ENUM>", ordinal = <EnumOrdinal>)
    lazy val staticEnumFields: List[ValDef] = enumDefs.map { enumDef =>
      ValDef(
        mods = Modifiers(ENUM),
        name = newTermName(enumDef.name),
        tpt = Ident(className),
        rhs = enumDef.tree)
    }

    lazy val arrayValues = enumDefs.map(d => enumIdent(d))

    // private val $VALUES: Array[<EnumClass>] = Array(<ENUM>, ...)
    lazy val staticValuesField: ValDef =
      q"private[this] val $$VALUES: Array[$className] = Array[$className](..$arrayValues)"

    // def values: Array[<EnumClass>] = $VALUES.clone()
    lazy val staticValuesMethod: DefDef =
      q"def values: Array[$className] = $$VALUES.clone()"

    // def valueOf(name: String): <EnumClass> = Enum.valueOf(<EnumClass>, name)
    lazy val staticValueOfMethod: DefDef =
      q"def valueOf(name: String): $className = Enum.valueOf(classOf[$className], name)"

    // java.lang.Enum[<EnumClass>]
    lazy val javaLangEnumType =
      tq"""java.lang.Enum[$className]"""

    // def <init>(name: String, ordinal: Int) = super.<init>(name, ordinal)
    lazy val classConstructor =
      q"""private def ${nme.CONSTRUCTOR}(name: String, ordinal: Int) = { super.${nme.CONSTRUCTOR}(name, ordinal); () }"""

    // def <init>() = super.<init>()
    lazy val objectConstructor =
      q"""private def ${nme.CONSTRUCTOR}() = { super.${nme.CONSTRUCTOR}(); () }"""

    val newClassBody: List[Tree] = classConstructor :: Nil
    val newClassTemplate = Template(List(javaLangEnumType), template.self, newClassBody)
    val newClassMods = Modifiers(classDef.mods.flags | ENUM)
    val newClassDef = ClassDef(newClassMods, classDef.name, classDef.tparams, newClassTemplate)

    val newObjectBody: List[Tree] = objectConstructor :: (staticEnumFields ++ (staticValuesField :: staticValuesMethod :: staticValueOfMethod :: Nil))
    val newObjectTemplate = Template(parents, template.self, newObjectBody)
    val newObjectDef = ModuleDef(Modifiers(ENUM), classDef.name.toTermName, newObjectTemplate)

    println(show(newClassDef))
    println(show(newObjectDef))

    c.Expr[Any](Block(List(newClassDef, newObjectDef), Literal(Constant(()))))
  }
}
