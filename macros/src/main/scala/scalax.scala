import language.experimental.macros
import scala.reflect.macros.Context

package object scalax {

  type Enum(values: _*) = macro Macros.enum

  object Macros {

    def enum(c: Context)(values: c.Tree*): c.Tree = {
      import c.universe._
      import Flag._

      case class EnumDef(ordinal: Int, name: String, tree: Tree)

      def enumInstance(ordinal: Int, name: String) = {
        val tree =
          Apply(
            New(c.enclosingClass.tpe), // Wrong!
            List(Literal(Constant(ordinal)), Literal(Constant(name)))
          )
        new EnumDef(ordinal, name, tree)
      }

      lazy val enumDefs = values.toList.collect {
        case Ident(TermName(name)) => enumInstance(-1, name)
        //case Apply(Ident(TermName(name)), List(Literal(Constant(desc)))) => enumInstance(name, name)
        //case Apply(Ident(TermName(id)), List(Block(_))) => enumInstance(id, "blocky")
      }

      // <ENUM> ===> @static val <ENUM>: <EnumClass> = new <EnumClass>(ordinal = 0, name = "<ENUM>")
      lazy val staticFields: List[ValDef] = enumDefs.map { enumDef =>
        ValDef(
            mods = Modifiers(PRIVATE /*, STATIC */),
            name = TermName(enumDef.name),
            tpt = TypeTree(),
            rhs = enumDef.tree
        )
      }

      // @static private val values: Array[<EnumClass>] = Array(<ENUM>, ...)
      lazy val privateStaticValuesField: ValDef =
        ValDef(
          mods = Modifiers(PRIVATE /*, STATIC */),
          name = TermName("$VALUES"),
          tpt = TypeTree(),
          rhs = ???
        )

      // @static def values: Array[<EnumClass>] = $VALUES.clone()
      lazy val staticValuesMethod: DefDef = ???

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
                  tpt = Ident(TypeName("String")),
                  rhs = EmptyTree
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

      val stdConstructor =
        DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))

      val generatedCode = /* staticFields ++ privateStaticValuesField :: staticValuesMethod :: */ List(staticValueOfMethod)

      val Template(_, _, _ :: existingCode) = c.enclosingTemplate

      Template(List(Ident(TypeName("AnyRef"))), emptyValDef, generatedCode ++ existingCode :+ stdConstructor)
    }
  }
}
