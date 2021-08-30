package io.github.edadma.enums

import scala.util.parsing.input.Position

case class Ident(pos: Position, s: String)
case class Constant(name: Ident, value: Option[String])

trait EnumsAST
case class EnumsDeclarationsAST(enums: List[EnumDeclarationAST]) extends EnumsAST
case class EnumDeclarationAST(name: Option[Ident], constants: List[Constant])
