import language.experimental.macros
import scala.reflect.macros.Context

package object scalax {

  type Enum(values: _*) = macro Macros.enum

  object Macros {

    def enum(c: Context)(values: c.Tree*): c.Tree = {
      import c.universe._
      import Flag._

      val ClassDef(_, className, _, _) = c.enclosingClass

      case class EnumDef(ordinal: Int, name: String, tree: Tree)

      def enumInstance(name: String, ordinal: Int) = {
        val tree =
          Apply(
            New(Ident(className)),
            List(Literal(Constant(name)), Literal(Constant(ordinal)))
          )
        new EnumDef(ordinal, name, tree)
      }


      lazy val enumDefs = values.toList.collect {
        // <ENUM>
        case Ident(TermName(name)) => enumInstance(name, -1)
        // <ENUM>(<enumParam>, ...)
        // <ENUM> { <enumDef>, ... }
        // <ENUM>(<enumParam>, ...) { <enumDef>, ... }
      }

      // <ENUM> ===> @static val <ENUM>: <EnumClass> = new <EnumClass>(ordinal = 0, name = "<ENUM>")
      /* error: scalax.Days does not take parameters */
      lazy val staticEnumFields: List[ValDef] = enumDefs.map { enumDef =>
        ValDef(
            mods = Modifiers(PRIVATE /*, STATIC */),
            name = TermName(enumDef.name),
            tpt  = TypeTree(),
            rhs  = enumDef.tree
        )
      }

      // @static private val $VALUES: Array[<EnumClass>] = Array(<ENUM>, ...)
      lazy val privateStaticValuesField: ValDef =
        ValDef(
          mods = Modifiers(PRIVATE /*, STATIC */),
          name = TermName("$VALUES"),
          tpt  = AppliedTypeTree(Select(Ident(TermName("scala")), TypeName("Array")), List(Ident(className))),
          // TODO: Currently Array()
          rhs  = Apply(Apply(TypeApply(Select(Select(Ident(TermName("scala")), TermName("Array")), TermName("apply")), List(Ident(className))), List()), List(Select(Ident(TermName("Predef")), TermName("implicitly"))))
        )

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
          rhs  = Apply(Select(Ident(TermName("$VALUES")), TermName("clone")), List()))

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
          tpt = TypeTree(),
          rhs =
            Apply(
              Select(Select(Select(Ident(TermName("java")), TermName("lang")), TermName("Enum")), TermName("valueOf")),
              List(Literal(Constant(c.enclosingClass.tpe)), Ident(TermName("name")))
            )
        )

      // extends java.lang.Enum[<EnumClass>]
      val extendsEnum =
        AppliedTypeTree(
          tpt  = Select(Select(Ident(TermName("java")), TermName("lang")), TypeName("Enum")),
          args = List(Ident(className))
        )

      // with scala.math.Ordered[<EnumClass>]
      /* error: overriding method compareTo in class Enum of type (x$1: Foo)Int;
       * method compareTo in trait Ordered of type (that: Foo)Int cannot override final member */
      val withOrdered =
        AppliedTypeTree(Select(Select(Ident(TermName("scala")), TermName("math")), TypeName("Ordered")), List(c.enclosingClass))

      // new <EnumClass>(name: String, ordinal) invokes new java.lang.Enum(name, ordinal)
      val enumConstructors =
        List(
          ValDef(
            mods = Modifiers(PRIVATE | LOCAL /*| PARAMACCESSOR*/),
            name = TermName("name"),
            tpt  = Ident(TypeName("String")),
            rhs  = EmptyTree
          ),
          ValDef(
            mods = Modifiers(PRIVATE | LOCAL /*| PARAMACCESSOR*/),
            name = TermName("ordinal"),
            tpt  = Ident(TypeName("Int")),
            rhs  = EmptyTree
          ),
          DefDef(
            mods = Modifiers(),
            name = nme.CONSTRUCTOR,
            tparams  = List(),
            vparamss =
              List(
                List(
                  ValDef(
                    mods = Modifiers(PARAM /*| PARAMACCESSOR*/),
                    name = TermName("name"),
                    tpt  = Ident(TypeName("String")),
                    rhs  = EmptyTree
                  ),
                  ValDef(
                    mods = Modifiers(PARAM /*| PARAMACCESSOR*/),
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
                  Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List(Ident(TermName("name")), Ident(TermName("ordinal"))))
                ),
                Literal(Constant(()))
              )
          )
        )

      val stdConstructor =
        DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))

      val generatedCode = /*staticEnumFields ++*/ (privateStaticValuesField :: staticValuesMethod :: staticValueOfMethod :: Nil)

      val Template(_, _, _ :: existingCode) = c.enclosingTemplate

      Template(List(extendsEnum), emptyValDef, generatedCode ++ enumConstructors ++ existingCode /*:+ stdConstructor*/)
    }
  }
}
