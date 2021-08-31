package io.github.edadma.enums

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.{CharSequenceReader, Position, Positional}

object EnumsParser extends RegexParsers with PackratParsers {

  override protected val whiteSpace: Regex = """(\s|/\*(.|[\r\n])*?\*/|//.*)+""".r

  lazy val pos: PackratParser[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  def kw(s: String): Regex = s"$s\\b".r

  lazy val enums: PackratParser[EnumsDeclarationsAST] =
    rep(enum) ^^ EnumsDeclarationsAST

  lazy val enum: PackratParser[EnumDeclarationAST] =
    kw("enum") ~ opt(ident) ~ "{" ~ repsep(enumConstant, ",") ~ opt(",") ~ "}" ~ ";" ^^ {
      case _ ~ n ~ _ ~ cs ~ _ ~ _ ~ _ => EnumDeclarationAST(n, cs)
    }

  lazy val enumConstant: PackratParser[EnumConstant] =
    ident ~ opt("=" ~> value) ^^ {
      case p ~ v => EnumConstant(p, v)
    }

  lazy val value: PackratParser[String] =
    """0x[0-9a-fA-F]+|[0-9]+""".r ^^ identity

  lazy val ident: PackratParser[Ident] =
    pos ~ "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ {
      case p ~ n => Ident(p, n)
    }

  def parseHeader(input: String): EnumsDeclarationsAST =
    parseAll(phrase(enums), new PackratReader(new CharSequenceReader(input))) match {
      case Success(result, _)     => result
      case NoSuccess(error, rest) => problem(rest.pos, error)
    }

}
