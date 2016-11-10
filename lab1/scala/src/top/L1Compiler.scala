package top

import java.io.{IOException,PrintWriter,FileWriter}
import frontend.{L1Parser, L1Scanner}
import backend.{Generator, Assembly}

/**
 *  The L1Compiler application object.
 *  Connects the different parts of the compiler to a pipeline.
 * @author Robin Steiger
 * @revision Andre Platzer
 */
object L1Compiler {

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      Report.fatalError("usage: scala top.L1Compiler <file.l1>")
    }
    
    val sourceFile = args(0)
    
    if (sourceFile.toLowerCase endsWith ".l1") {
      val destFile = (sourceFile dropRight 3) + ".s"
      compile(sourceFile, destFile)
    }
    else {
      Report.fatalError("unrecognized file format: " + sourceFile)
    }
  }

// attemps to compile a l1 source file to a x86-64 assembly file.
  private def compile(sourceFile: String, destFile: String) {
    try {
      Report.numErrors = 0
      L1Parser.program(new L1Scanner.Scanner(FileReader(sourceFile))) match {
        case L1Parser.Success(program, _) =>
          // Check for undeclared variables, missing returns, ...
          Analyzer.verifyProgram(program)
          
          if (Report.numErrors == 0) {
            val code = Generator.generate(program)
            
            val out = new PrintWriter(new FileWriter(destFile))
            out.println(code)
            out.flush
            out.close
          } else {
            println(Report.numErrors + " errors found.")
			System.exit(-1)
          }
        case failure @ L1Parser.NoSuccess(_, _) =>
          println(failure)
		  System.exit(-1)
      }
    }
    catch {
      case e: IOException =>
        Report.error(e.getMessage())
		System.exit(-1)
    }
    finally {
    if (Report.numErrors > 0) {
      Report.fatalError("Compilation failed.")
      System.exit(-1)
    }
}
  }
}
