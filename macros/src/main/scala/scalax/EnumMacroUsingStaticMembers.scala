package scalax

import scala.reflect.macros.Context

object EnumMacroUsingStaticMembers {
  def apply(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    val EnumValue         = 1L << 48
    val ENUM              = EnumValue.asInstanceOf[FlagSet]
    val StaticValue       = 1L << 23
    val STATIC            = StaticValue.asInstanceOf[FlagSet]
    val StableValue       = 1L << 22
    val STABLE            = StableValue.asInstanceOf[FlagSet]
    val JavaValue   = 1L << 20
    val JAVA        = JavaValue.asInstanceOf[FlagSet]

    val List(Expr(classDef @ ClassDef(_, className, _, template))) = annottees
    val Template(parents, self, body) = template

    case class EnumDef(ordinal: Int, name: String, tree: Tree)

    def enumInstance(name: String, ordinal: Int): EnumDef =
      EnumDef(ordinal, name, q"new $className($name, $ordinal)")

    lazy val enumDefs: List[EnumDef] = body.zipWithIndex.toList.collect {
      // <ENUM>
      case (Ident(termName: TermName), index) => enumInstance(termName.encoded, index)
      // <ENUM>(<enumParam>, ...)
      // <ENUM> { <enumDef>, ... }
      // <ENUM>(<enumParam>, ...) { <enumDef>, ... }
    }

    def enumIdent(enumDef: EnumDef) = Ident(TermName(enumDef.name))

    // <ENUM> ===> val <ENUM>: <EnumClass> = new <EnumClass>(name = "<ENUM>", ordinal = <EnumOrdinal>)
    lazy val staticEnumFields: List[ValDef] = enumDefs.map { enumDef =>
      ValDef(
        mods = Modifiers(ENUM | STATIC | STABLE | JAVA),
        name = TermName(enumDef.name),
        tpt = Ident(className),
        rhs = enumDef.tree)
    }

    // private val $VALUES: Array[<EnumClass>] = Array(<ENUM>, ...)
    lazy val staticValuesField: ValDef =
      ValDef(
        mods = Modifiers(PRIVATE | LOCAL | STATIC),
        name = TermName("$VALUES"),
        tpt = AppliedTypeTree(Select(Ident(TermName("scala")), TypeName("Array")), List(Ident(className))),
        rhs =
          Apply(
            Apply(
              TypeApply(
                Select(Select(Ident(TermName("scala")), TermName("Array")), TermName("apply")),
                List(Ident(className))),
              List(enumDefs.map(d => enumIdent(d)): _*)),
            List(Select(Ident(TermName("Predef")), TermName("implicitly")))))

    // def values: Array[<EnumClass>] = $VALUES.clone()
    lazy val staticValuesMethod: DefDef =
      DefDef(
        mods = Modifiers(STATIC),
        name = TermName("values"),
        tparams = List(),
        vparamss = List(),
        tpt =
          AppliedTypeTree(
            tpt = Select(Ident(TermName("scala")), TypeName("Array")),
            args = List(Ident(className))),
        rhs = Apply(Select(Ident(TermName("$VALUES")), TermName("clone")), List()))

    // def valueOf(name: String): <EnumClass> = Enum.valueOf(<EnumClass>, name)
    lazy val staticValueOfMethod: DefDef =
      DefDef(
        mods = Modifiers(STATIC),
        name = TermName("valueOf"),
        tparams = List(),
        vparamss =
          List(
            List(
              ValDef(
                mods = Modifiers(PARAM),
                name = TermName("name"),
                tpt = Ident(TypeName("String")),
                rhs = EmptyTree))),
        tpt = Ident(className),
        rhs =
          Apply(
            Select(Select(Select(Ident(TermName("java")), TermName("lang")), TermName("Enum")), TermName("valueOf")),
            List(TypeApply(Ident(TermName("classOf")), List(Ident(className))), Ident(TermName("name")))))

    // java.lang.Enum[<EnumClass>]
    lazy val javaLangEnumType =
      tq"""java.lang.Enum[$className]"""

    // def <init>(name: String, ordinal: Int) = super.<init>(name, ordinal)
    lazy val classConstructor =
      q"""private def ${nme.CONSTRUCTOR}(nameX: String, ordinal: Int) = { super.${nme.CONSTRUCTOR}(nameX, ordinal); () }"""

    // def <init>() = super.<init>()
    lazy val objectConstructor =
      q"""private def ${nme.CONSTRUCTOR}() = { super.${nme.CONSTRUCTOR}(); () }"""

    val newClassBody: List[Tree] = classConstructor :: (staticEnumFields ++ (staticValuesField :: staticValuesMethod :: staticValueOfMethod :: Nil))
    val newClassTemplate = Template(List(javaLangEnumType), template.self, newClassBody)
    val newClassMods = Modifiers(classDef.mods.flags | FINAL | ENUM)
    val newClassDef = ClassDef(newClassMods, classDef.name, classDef.tparams, newClassTemplate)

    lazy val staticForwarders: List[Tree] = Nil
//      enumDefs.map { enumDef =>
//      DefDef(
//        mods = Modifiers(STABLE),
//        name = TermName(enumDef.name),
//        tparams = List(),
//        vparamss = List(),
//        tpt = Ident(className),
//        rhs = Select(Ident(classDef.linkedClassOfClass.name), enumDef.name)
//      )
//    }

    val newObjectBody: List[Tree] = objectConstructor :: staticForwarders
    val newObjectTemplate = Template(Nil, template.self, newObjectBody)
    val newObjectDef = ModuleDef(Modifiers(), classDef.name.toTermName, newObjectTemplate)

    println(show(newClassDef))
    println(show(newObjectDef))

    c.Expr[Any](Block(List(newClassDef, newObjectDef), Literal(Constant(()))))
  }
}
