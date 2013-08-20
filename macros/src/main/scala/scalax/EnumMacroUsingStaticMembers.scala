package scalax

import scala.reflect.macros.Context

object EnumMacroUsingStaticMembers {
  def apply(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    val EnumValue         = 1L << 48
    val ENUM              = EnumValue.asInstanceOf[FlagSet]
    val ArtifactValue     = 1L << 46
    val ARTIFACT          = ArtifactValue.asInstanceOf[FlagSet]
    val CaseAccessorValue = 1L << 24
    val CASEACCESSOR      = CaseAccessorValue.asInstanceOf[FlagSet]
    val StaticValue       = 1L << 23
    val STATIC            = StaticValue.asInstanceOf[FlagSet]
    val StableValue       = 1L << 22
    val STABLE            = StableValue.asInstanceOf[FlagSet]
    val JavaValue         = 1L << 20
    val JAVA              = JavaValue.asInstanceOf[FlagSet]

    val List(Expr(classDef @ ClassDef(_, className, _, template))) = annottees
    val Template(parents, self, body) = template

    case class EnumDef(ordinal: Int, name: String, tree: Tree)

    def enumInstance(name: String, ordinal: Int): EnumDef = {
      val tree =
        Apply(
          Select(New(Ident(className)), nme.CONSTRUCTOR),
          List(Literal(Constant(name)), Literal(Constant(ordinal))))
      new EnumDef(ordinal, name, tree)
    }

    lazy val enumDefs: List[EnumDef] = body.zipWithIndex.toList.collect {
      // <ENUM>
      case (Ident(termName: TermName), index) => enumInstance(termName.encoded, index)
      // <ENUM>(<enumParam>, ...)
      // <ENUM> { <enumDef>, ... }
      // <ENUM>(<enumParam>, ...) { <enumDef>, ... }
    }

    def enumIdent(enumDef: EnumDef) = Ident(newTermName(enumDef.name))

    // <ENUM> ===> val <ENUM>: <EnumClass> = new <EnumClass>(name = "<ENUM>", ordinal = <EnumOrdinal>)
    lazy val staticEnumFields: List[ValDef] = enumDefs.map { enumDef =>
      ValDef(
        mods = Modifiers(ENUM | STATIC | JAVA | STABLE),
        name = newTermName(enumDef.name),
        tpt = Ident(className),
        rhs = enumDef.tree)
    }

    // private val $VALUES: Array[<EnumClass>] = Array(<ENUM>, ...)
    lazy val staticValuesField: ValDef =
      ValDef(
        mods = Modifiers(PRIVATE | LOCAL | STATIC | JAVA),
        name = newTermName("$VALUES"),
        tpt = AppliedTypeTree(Select(Ident(newTermName("scala")), newTypeName("Array")), List(Ident(className))),
        rhs =
          Apply(
            Apply(
              TypeApply(
                Select(Select(Ident(newTermName("scala")), newTermName("Array")), newTermName("apply")),
                List(Ident(className))),
              List(enumDefs.map(d => enumIdent(d)): _*)),
            List(Select(Ident(newTermName("Predef")), newTermName("implicitly")))))

    // def values: Array[<EnumClass>] = $VALUES.clone()
    lazy val staticValuesMethod: DefDef =
      DefDef(
        mods = Modifiers(STATIC | JAVA),
        name = newTermName("values"),
        tparams = List(),
        vparamss = List(),
        tpt =
          AppliedTypeTree(
            tpt = Select(Ident(newTermName("scala")), newTypeName("Array")),
            args = List(Ident(className))),
        rhs = Apply(Select(Ident(newTermName("$VALUES")), newTermName("clone")), List()))

    // def valueOf(name: String): <EnumClass> = Enum.valueOf(<EnumClass>, name)
    lazy val staticValueOfMethod: DefDef =
      DefDef(
        mods = Modifiers(STATIC | JAVA),
        name = newTermName("valueOf"),
        tparams = List(),
        vparamss =
          List(
            List(
              ValDef(
                mods = Modifiers(PARAM),
                name = newTermName("name"),
                tpt = Ident(newTypeName("String")),
                rhs = EmptyTree))),
        tpt = Ident(className),
        rhs =
          Apply(
            Select(Select(Select(Ident(newTermName("java")), newTermName("lang")), newTermName("Enum")), newTermName("valueOf")),
            List(TypeApply(Ident(newTermName("classOf")), List(Ident(className))), Ident(newTermName("name")))))

    // java.lang.Enum[<EnumClass>]
    lazy val javaLangEnumType =
      AppliedTypeTree(
        tpt = Select(Select(Ident(newTermName("java")), newTermName("lang")), newTypeName("Enum")),
        args = List(Ident(className)))

    // def <init>(name: String, ordinal) = super.<init>(name, ordinal)
    lazy val classConstructor =
      DefDef(
        mods = Modifiers(PRIVATE | FINAL | JAVA),
        name = nme.CONSTRUCTOR,
        tparams = List(),
        vparamss =
          List(
            List(
              ValDef(
                mods = Modifiers(PARAM),
                name = newTermName("name"),
                tpt = Ident(newTypeName("String")),
                rhs = EmptyTree),
              ValDef(
                mods = Modifiers(PARAM),
                name = newTermName("ordinal"),
                tpt = Ident(newTypeName("Int")),
                rhs = EmptyTree))),
        tpt = TypeTree(),
        rhs =
          Block(
            List(
              Apply(
                Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
                List(Ident(newTermName("name")), Ident(newTermName("ordinal"))))),
            Literal(Constant(()))))

    val newClassBody: List[Tree] = classConstructor :: (staticEnumFields ++ (staticValuesField :: staticValuesMethod :: staticValueOfMethod :: Nil))
    val newClassTemplate = Template(List(javaLangEnumType), template.self, newClassBody)
    val newClassMods = Modifiers(classDef.mods.flags | ENUM | JAVA)
    val newClassDef = ClassDef(newClassMods, classDef.name, classDef.tparams, newClassTemplate)

    println(show(newClassDef))

    c.Expr[Any](newClassDef)
  }
}
