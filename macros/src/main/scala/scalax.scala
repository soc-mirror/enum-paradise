import language.experimental.macros
import scala.reflect.macros.Context

package object scalax {

  type Enum(values: _*) = macro Macros.enum

  object Macros {

    def enum(c: Context)(values: c.Tree*): c.Tree = {
      import c.universe._
      import Flag._

      val ClassDef(_, className, _, _) = c.enclosingClass

      val STATIC = 1 << 23
      val PARAMACCESSOR = 1 << 29
      def setFlag(symbol: Symbol, flag: Long) {
        val compilerSymbol = symbol.asInstanceOf[scala.tools.nsc.Global#Symbol]
        compilerSymbol.setFlag(flag)
      }
      def printFlags(symbol: Symbol) {
        println(symbol.asInstanceOf[scala.tools.nsc.Global#Symbol].flagString)
      }

      case class EnumDef(ordinal: Int, name: String, tree: Tree)

      def enumInstance(name: String, ordinal: Int) = {
        val tree =
          Apply(
            Select(New(Ident(className)), nme.CONSTRUCTOR),
            List(Literal(Constant(name)), Literal(Constant(ordinal)))
          )
        new EnumDef(ordinal, name, tree)
      }

      lazy val enumDefs = values.zipWithIndex.toList.collect {
        // <ENUM>
        case (Ident(TermName(name)), index) => enumInstance(name, index)
        // <ENUM>(<enumParam>, ...)
        // <ENUM> { <enumDef>, ... }
        // <ENUM>(<enumParam>, ...) { <enumDef>, ... }
      }

      // <ENUM> ===> @static val <ENUM>: <EnumClass> = new <EnumClass>(name = "<ENUM>", ordinal = <EnumOrdinal>)
      /* error: scalax.Days does not take parameters */
      lazy val staticEnumFields: List[ValDef] = enumDefs.map { enumDef =>
        ValDef(
            mods = Modifiers(PRIVATE /*, STATIC */),
            name = TermName(enumDef.name),
            tpt  = Ident(className),
            rhs  = enumDef.tree
        )
      }
      //staticEnumFields.foreach(enum => setFlag(enum.symbol, STATIC))

      // @static private val $VALUES: Array[<EnumClass>] = Array(<ENUM>, ...)
      lazy val privateStaticValuesField: ValDef =
        ValDef(
          mods = Modifiers(PRIVATE | LOCAL /*, STATIC */),
          name = TermName("$VALUES"),
          tpt  = AppliedTypeTree(Select(Ident(TermName("scala")), TypeName("Array")), List(Ident(className))),
          // TODO: Currently Array()
          rhs  =
            Apply(
              Apply(
                TypeApply(
                  Select(Select(Ident(TermName("scala")), TermName("Array")), TermName("apply")),
                  List(Ident(className))
                ),
                List(/* TODO */)
              ),
              List(Select(Ident(TermName("Predef")), TermName("implicitly")))
            )
        )
      //setFlag(privateStaticValuesField.symbol, STATIC)

      // @static def values: Array[<EnumClass>] = $VALUES.clone()
      lazy val staticValuesMethod: DefDef =
        DefDef(
          mods = Modifiers(/* STATIC */),
          name = TermName("values"),
          tparams  = List(),
          vparamss = List(),
          tpt  =
            AppliedTypeTree(
              tpt  = Select(Ident(TermName("scala")), TypeName("Array")),
              args = List(Ident(className))
            ),
          rhs  = Apply(Select(Ident(TermName("$VALUES")), TermName("clone")), List())
        )
      //setFlag(staticValuesMethod.symbol, STATIC)

      // @static def valueOf(name: String): <EnumClass> = Enum.valueOf(<EnumClass>, name)
      lazy val staticValueOfMethod: DefDef =
        DefDef(
          mods = Modifiers(/* STATIC */),
          name = TermName("valueOf"),
          tparams = List(),
          vparamss =
            List(
              List(
                ValDef(
                  mods = Modifiers(PARAM),
                  name = TermName("name"),
                  tpt  = Ident(TypeName("String")),
                  rhs  = EmptyTree
                )
              )
            ),
          tpt = Ident(className),
          rhs =
            Apply(
              Select(Select(Select(Ident(TermName("java")), TermName("lang")), TermName("Enum")), TermName("valueOf")),
              List(TypeApply(Ident(TermName("classOf")),List(Ident(className))), Ident(TermName("name")))
            )
        )
      //setFlag(staticValueOfMethod.symbol, STATIC)

      // extends java.lang.Enum[<EnumClass>]
      lazy val extendsEnum =
        AppliedTypeTree(
          tpt  = Select(Select(Ident(TermName("java")), TermName("lang")), TypeName("Enum")),
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
                    name = TermName("name"),
                    tpt  = Ident(TypeName("String")),
                    rhs  = EmptyTree
                  ),
                  ValDef(
                    mods = Modifiers(PARAM),
                    name = TermName("ordinal"),
                    tpt  = Ident(TypeName("Int")),
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
                    List(Ident(TermName("name")), Ident(TermName("ordinal"))))
                ),
                Literal(Constant(()))
              )
          )

      val generatedCode = enumConstructor :: staticEnumFields ++ (privateStaticValuesField :: staticValuesMethod :: staticValueOfMethod :: Nil)

      val Template(_, _, _ :: existingCode) = c.enclosingTemplate

      val template = Template(List(extendsEnum), emptyValDef, generatedCode ++ existingCode)
      println(show(template))
      template
    }
  }
}
