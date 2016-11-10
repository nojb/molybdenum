package top
 
import scala.util.parsing.input.Position

/**
 * The class centralising reporting of errors.
 * @author Robin Steiger
 */
object Report {
  
  /** The number of errors so far. */
  var numErrors: Int = 0
  
  /** Prints out an error message. */
  def error(message: String) {
    numErrors += 1
    println(message)
  }
  
  /** Prints out an error message. */
  def error(position: Position, message: String) {
    numErrors += 1
    println(position.line + "." + position.column + ":  " + message)
    println(position.longString)
  }

  /** Prints out an error message and stops the program execution. */
  def fatalError(message: String): Nothing = {
    numErrors += 1
    error(message)
    System.exit(-1)
    throw new Error
  }
  
  /** Prints out an error message and stops the program execution. */
  def fatalError(position: Position, message: String): Nothing = {
    numErrors += 1
    error(position, message)
    System.exit(-1)
    throw new Error
  }

}
