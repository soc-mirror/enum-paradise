package scalax

import language.experimental.macros

class enum extends annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro EnumMacro.apply
}
