package io.github.edadma.enums

import io.github.edadma.json
import scopt.OParser
import io.github.edadma.mustache._

import java.io.File
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Main extends App {
  case class Config(file: File, start: Int, end: Option[Int])

  val builder = OParser.builder[Config]
  val parser = {
    import builder._

    OParser.sequence(
      programName("enums"),
      head("Facade Enum Generator", "v0.1.0"),
      opt[Option[Int]]('e', "end")
        .optional()
        .action((e, c) => c.copy(end = e))
        .text("start line number (optional)"),
      help('h', "help").text("prints this usage text"),
      opt[Int]('s', "start")
        .optional()
        .action((s, c) => c.copy(start = s))
        .text("start line number (optional)"),
      version('v', "version").text("prints the version"),
      arg[File]("<file>")
        .required()
        .action((f, c) => c.copy(file = f))
        .validate(f =>
          if (f.exists && f.isFile && f.canRead) success
          else failure("<file> must exist and be a readable file"))
        .text("path to text file to open")
    )
  }

  OParser.parse(parser, args, Config(null, 1, None)) match {
    case Some(conf) => app(conf)
    case _          =>
  }

  def app(conf: Config): Unit = {
    val lines                       = util.Using(scala.io.Source.fromFile(conf.file.getPath))(_.getLines() to ArraySeq).get
    val section                     = lines dropRight (lines.length - conf.end.getOrElse(0)) drop (conf.start - 1)
    val (defines, text)             = preprocess(section)
    val EnumsDeclarationsAST(enums) = EnumsParser.parseHeader(text mkString)
    var nonames: Int                = 0
    val list                        = new ListBuffer[(String, json.Array)]

    for (EnumDeclarationAST(name, constants) <- enums) {
      val enumname =
        if (name.isDefined) name.get.s
        else {
          nonames += 1
          s"_$nonames"
        }

      list += (enumname -> enumConstants(enumname, constants))
    }

    val data = json.Object("enums" -> json.Array(list.map {
      case (e, cs) => json.Object("name" -> e, "constants" -> cs)
    }))

    val template =
      """
        |{{#enums}}
        |class {{name}}(val value: CInt) extends AnyVal
        |object {{name}} {
        |{{#constants}}
        |  final val {{name}} = new {{_._.name}}({{value}})
        |{{/constants}}
        |}
        |
        |{{/enums}}
        |""".trim.stripMargin

    println(processMustache(data, template, "trim" -> false, "removeNonSectionBlanks" -> false))
  }

  def enumConstants(enum: String, constants: List[EnumConstant]): json.Array = {
    var next: Int = 0
    val buf       = new ListBuffer[(String, String)]
    var hex       = false

    for (EnumConstant(Ident(_, name), value) <- constants) {
      if (value.isDefined)
        hex = value.get startsWith "0x"

      if (value.isDefined)
        next = Integer.parseInt(value.get.substring(if (hex) 2 else 0), if (hex) 16 else 10)

      val const = if (hex) s"0x${next.toHexString}" else next.toString

      next += 1
      buf += (name -> const)
    }

    json.Array(buf map { case (n, v) => json.Object("name" -> n, "value" -> v) })
  }

  def preprocess(lines: Seq[String]): (Map[String, String], Seq[String]) = {
    val defines         = mutable.HashMap[String, String]()
    val buf             = new ArrayBuffer[String]
    val defineDirective = """#define\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+(.*)""".r

    for (l <- lines)
      l.trim match {
        case defineDirective(name, definition) => defines(name) = definition
        case s                                 => buf += s
      }

    (defines.toMap, buf map (_ :+ '\n') to ArraySeq)
  }

}
