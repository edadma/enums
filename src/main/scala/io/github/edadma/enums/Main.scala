package io.github.edadma.enums

import scopt.OParser

import java.io.File

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
    println(conf)
  }

}
