import language.experimental.macros
import scala.reflect.macros.Context

package object scalax {

  class Enum extends annotation.StaticAnnotation {
    def macroTransform(annottees: Any*) = macro EnumMacro.impl
  }

  object EnumMacro {

    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      import Flag._

      val STATIC = 1 << 23
      val PARAMACCESSOR = 1 << 29
      def setFlag(symbol: Symbol, flag: Long) {
        val compilerSymbol = symbol.asInstanceOf[scala.tools.nsc.Global#Symbol]
        compilerSymbol.setFlag(flag)
      }
      def printFlags(symbol: Symbol) {
        println(symbol.asInstanceOf[scala.tools.nsc.Global#Symbol].flagString)
      }

      val List(Expr(classDef @ ClassDef(_, className, _, template))) = annottees
      val Template(parents, self, body) = template

      case class EnumDef(ordinal: Int, name: String, tree: Tree)

      def enumInstance(name: String, ordinal: Int) = {
        val tree =
          Apply(
            Select(New(Ident(className)), nme.CONSTRUCTOR),
            List(Literal(Constant(name)), Literal(Constant(ordinal)))
          )
        new EnumDef(ordinal, name, tree)
      }

      lazy val enumDefs = body.zipWithIndex.toList.collect {
        // <ENUM>
        case (Ident(termName: TermName), index)  => enumInstance(termName.encoded, index)
        // <ENUM>(<enumParam>, ...)
        // <ENUM> { <enumDef>, ... }
        // <ENUM>(<enumParam>, ...) { <enumDef>, ... }
      }

      // <ENUM> ===> @static val <ENUM>: <EnumClass> = new <EnumClass>(name = "<ENUM>", ordinal = <EnumOrdinal>)
      /* error: scalax.Days does not take parameters */
      lazy val staticEnumFields: List[ValDef] = enumDefs.map { enumDef =>
        ValDef(
            mods = Modifiers(PRIVATE /*, STATIC */),
            name = newTermName(enumDef.name),
            tpt  = Ident(className),
            rhs  = enumDef.tree
        )
      }
      staticEnumFields.foreach(enum => setFlag(enum.symbol, STATIC))

      // @static private val $VALUES: Array[<EnumClass>] = Array(<ENUM>, ...)
      lazy val privateStaticValuesField: ValDef =
        ValDef(
          mods = Modifiers(PRIVATE | LOCAL /*, STATIC */),
          name = newTermName("$VALUES"),
          tpt  = AppliedTypeTree(Select(Ident(newTermName("scala")), newTypeName("Array")), List(Ident(className))),
          // TODO: Currently Array()
          rhs  =
            Apply(
              Apply(
                TypeApply(
                  Select(Select(Ident(newTermName("scala")), newTermName("Array")), newTermName("apply")),
                  List(Ident(className))
                ),
                List(/* TODO */)
              ),
              List(Select(Ident(newTermName("Predef")), newTermName("implicitly")))
            )
        )
      setFlag(privateStaticValuesField.symbol, STATIC)

      // @static def values: Array[<EnumClass>] = $VALUES.clone()
      lazy val staticValuesMethod: DefDef =
        DefDef(
          mods = Modifiers(/* STATIC */),
          name = newTermName("values"),
          tparams  = List(),
          vparamss = List(),
          tpt  =
            AppliedTypeTree(
              tpt  = Select(Ident(newTermName("scala")), newTypeName("Array")),
              args = List(Ident(className))
            ),
          rhs  = Apply(Select(Ident(newTermName("$VALUES")), newTermName("clone")), List())
        )
      setFlag(staticValuesMethod.symbol, STATIC)

      // @static def valueOf(name: String): <EnumClass> = Enum.valueOf(<EnumClass>, name)
      lazy val staticValueOfMethod: DefDef =
        DefDef(
          mods = Modifiers(/* STATIC */),
          name = newTermName("valueOf"),
          tparams = List(),
          vparamss =
            List(
              List(
                ValDef(
                  mods = Modifiers(PARAM),
                  name = newTermName("name"),
                  tpt  = Ident(newTypeName("String")),
                  rhs  = EmptyTree
                )
              )
            ),
          tpt = Ident(className),
          rhs =
            Apply(
              Select(Select(Select(Ident(newTermName("java")), newTermName("lang")), newTermName("Enum")), newTermName("valueOf")),
              List(TypeApply(Ident(newTermName("classOf")),List(Ident(className))), Ident(newTermName("name")))
            )
        )
      setFlag(staticValueOfMethod.symbol, STATIC)

      // extends java.lang.Enum[<EnumClass>]
      lazy val extendsEnum =
        AppliedTypeTree(
          tpt  = Select(Select(Ident(newTermName("java")), newTermName("lang")), newTypeName("Enum")),
          args = List(Ident(className))
        )

      // new <EnumClass>(name: String, ordinal) invokes new java.lang.Enum(name, ordinal)
      lazy val enumConstructor =
          DefDef(
            mods = Modifiers(PRIVATE),
            name = nme.CONSTRUCTOR,
            tparams  = List(),
            vparamss =
              List(
                List(
                  ValDef(
                    mods = Modifiers(PARAM),
                    name = newTermName("name"),
                    tpt  = Ident(newTypeName("String")),
                    rhs  = EmptyTree
                  ),
                  ValDef(
                    mods = Modifiers(PARAM),
                    name = newTermName("ordinal"),
                    tpt  = Ident(newTypeName("Int")),
                    rhs  = EmptyTree
                  )
                )
              ),
            tpt  = TypeTree(),
            rhs  =
              Block(
                List(
                  Apply(
                    Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
                    List(Ident(newTermName("name")), Ident(newTermName("ordinal"))))
                ),
                Literal(Constant(()))
              )
          )

      val generatedCode: List[Tree] = enumConstructor :: staticEnumFields ++ (privateStaticValuesField :: staticValuesMethod :: staticValueOfMethod :: Nil)

      val newTemplate = Template(List(extendsEnum), template.self, generatedCode)

      val newClassDef = ClassDef(classDef.mods, classDef.name, classDef.tparams, newTemplate)

      println(show(newClassDef))

      c.Expr[Any](newClassDef)
    }
  }
}
