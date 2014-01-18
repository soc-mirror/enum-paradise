package scalax

import scala.reflect.macros.blackbox.Context

object EnumMacro {
  def apply(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    val PARAMACCESSOR = (1L << 29).asInstanceOf[FlagSet]
    val STATIC        = (1L << 23).asInstanceOf[FlagSet]
    val STABLE        = (1L << 22).asInstanceOf[FlagSet]
    val JAVA          = (1L << 20).asInstanceOf[FlagSet]

    val List(Expr(classDef @ ClassDef(_, className, _, template))) = annottees
    val Template(parents, self, body) = template

    lazy val EnumType   = tq"java.lang.Enum[$className]"
    lazy val StringType = tq"java.lang.String"
    lazy val IntType    = tq"scala.Int"
    lazy val AnyRefType = tq"scala.AnyRef"

    case class EnumDef(ordinal: Int, name: String, tree: Tree)

    def enumInstance(name: String, ordinal: Int, args: List[Tree], body: List[Tree]): EnumDef = {
      val tree =
        if (body.isEmpty)
          q"new $className($name, $ordinal, ..$args)"
        else {
          val bodyWithOverrides = body.map(tree => addOverrideModifier(tree))
          q"new $className($name, $ordinal, ..$args) {..$bodyWithOverrides}"
        }
      EnumDef(ordinal, name, tree)
    }

    def addOverrideModifier(tree: Tree) = tree match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) => DefDef(Modifiers(mods.flags | OVERRIDE), name, tparams, vparamss, tpt, rhs)
    }


    lazy val constructorParams: List[ValDef] = {
      val result = body collectFirst {
        case DefDef(_, nme.CONSTRUCTOR, _, List(paramList), _, _) => paramList
      }
      result.get
    }

    lazy val enumDefs: List[EnumDef] = body.zipWithIndex.toList.collect {
      // <ENUM>
      case (Ident(termName: TermName), index) => enumInstance(termName.encoded, index, Nil, Nil)
      // <ENUM> { <enumDef>, ... }
      case (Apply(Ident(termName: TermName), List(Block(body, Literal(Constant(()))))), index) =>
        enumInstance(termName.encoded, index, Nil, body)
      // <ENUM>(<enumParam>, ...)
      case (Apply(Ident(termName: TermName), args), index) =>
        enumInstance(termName.encoded, index, args, Nil)
      // <ENUM>(<enumParam>, ...) { <enumDef>, ... }
    }

    def enumIdent(enumDef: EnumDef) = Ident(TermName(enumDef.name))

    // <ENUM> ===> val <ENUM>: <EnumClass> = new <EnumClass>(name = "<ENUM>", ordinal = <EnumOrdinal>)
    lazy val staticEnumFields: List[ValDef] = enumDefs.map { enumDef =>
      ValDef(
        mods = Modifiers(ENUM | STATIC | STABLE),
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
                tpt = StringType,
                rhs = EmptyTree))),
        tpt = Ident(className),
        rhs =
          Apply(
            Select(Select(Select(Ident(TermName("java")), TermName("lang")), TermName("Enum")), TermName("valueOf")),
            List(TypeApply(Ident(TermName("classOf")), List(Ident(className))), Ident(TermName("name")))))

    lazy val instanceFields = {
      val nameAccessor = ValDef(Modifiers(PRIVATE | LOCAL | PARAMACCESSOR), TermName("name"), StringType, EmptyTree)
      val ordinalAccessor = ValDef(Modifiers(PRIVATE | LOCAL | PARAMACCESSOR), TermName("ordinal"), IntType, EmptyTree)
      val otherAccessors = body collect {
        case accessor @ ValDef(Modifiers(PARAMACCESSOR, _, _), _, _, _) => accessor
      }
      nameAccessor :: ordinalAccessor :: otherAccessors
    }

    lazy val unapplyMethod = {
      if (constructorParams.nonEmpty) {
        val constructorNames = constructorParams.collect {
          case ValDef(_, name, _, _) => Select(Ident(TermName("x")), name)
        }
        val constructorTypes = constructorParams.collect {
          case ValDef(_, _, tpe, _) => tpe
        }
        val (unapplyType, unapplyBody) =
          if (constructorTypes.size == 1)
            (tq"scala.Some[..$constructorTypes]", q"scala.Some(..$constructorNames)")
          else
            (tq"scala.Some[(..$constructorTypes)]", q"scala.Some((..$constructorNames))")

        Some(
          q"def unapply(x: $className): $unapplyType = $unapplyBody")
      }
      else None
    }

    lazy val existingClassMethods = body.collect {
      case defDef @ DefDef(_, name, _, _, _, _) if name != nme.CONSTRUCTOR => defDef
    }

    // def <init>(name: String, ordinal: Int, <params>) = super.<init>(name, ordinal)
    lazy val instanceConstructor =
      q"""private def ${nme.CONSTRUCTOR}(name: String, ordinal: Int, ..${constructorParams}) = { super.${nme.CONSTRUCTOR}(name, ordinal); () }"""

    val newClassBody: List[Tree] = (instanceFields :+ instanceConstructor) ++ (staticEnumFields ++ (staticValuesField :: staticValuesMethod :: staticValueOfMethod :: Nil))
    val newClassTemplate = Template(EnumType :: parents.filterNot(_ equalsStructure AnyRefType), template.self, newClassBody ++ existingClassMethods)
    val newClassMods = Modifiers(classDef.mods.flags | ENUM)
    val newClassDef = ClassDef(newClassMods, className, classDef.tparams, newClassTemplate)

        // def <init>() = super.<init>()
    lazy val objectConstructor =
      q"""private def ${nme.CONSTRUCTOR}() = { super.${nme.CONSTRUCTOR}(); () }"""
    val newObjectBody: List[Tree] = objectConstructor :: unapplyMethod.toList
    val newObjectTemplate = Template(Nil, template.self, newObjectBody)
    val newObjectDef = ModuleDef(Modifiers(), className.toTermName, newObjectTemplate)

    println(showCode(newClassDef))
    println(showCode(newObjectDef))

    c.Expr[Any](Block(List(newClassDef, newObjectDef), Literal(Constant(()))))
  }
}
