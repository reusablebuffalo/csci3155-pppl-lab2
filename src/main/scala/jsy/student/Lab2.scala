package jsy.student

import com.sun.org.apache.xpath.internal.operations.Mult
import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   *
   * Ian Smith
   * 
   * Partner: Vy Le
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => if(n.isNaN) Double.NaN else n
      case B(b) => if(b) 1 else 0
      case S(s) => if(s == "") 0 else {try {s.toDouble} catch { case e: java.lang.NumberFormatException => Double.NaN}}
      case Undefined => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(0.0) => false
      case N(-0.0) => false
      case N(n) => if (n.isNaN) false else true
      case S(s) => if (s == "") false else true
      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case N(n) => if (n.isNaN) "NaN" else if (n.isWhole) "%d".format(n.toInt) else "%s".format(n)
      case B(b) => if(b) "true" else "false" // true or false
    }
  }

  def eval(env: Env, e: Expr): Expr = e match {
    /* Base Cases */
    case N(n) => N(n)
    case S(s) => S(s)
    case B(b) => B(b)
    case Undefined => Undefined
    case Var(x) => lookup(env, x)// returns looked up variable

    /* Inductive Cases */
    case Print(e1) => println(pretty(eval(env, e1))); Undefined

    /* Binary */
    case Binary(bop, e1, e2) => bop match {

      /* Binary Arithmetic Ops */
      case Plus => (eval(env,e1),eval(env,e2)) match {
        case (S(s1), S(s2)) => S(s1 + s2)
        case (S(s1), expr2) => S(s1 + toStr(expr2))
        case (expr1, S(s2)) => S(toStr(expr1) + s2)
        case (expr1, expr2) => N(toNumber(expr1) + toNumber(expr2))
      }
      case Minus => N(toNumber(eval(env,e1))-toNumber(eval(env,e2)))
      case Div => N(toNumber(eval(env,e1))/toNumber(eval(env, e2)))
      case Times => N(toNumber(eval(env,e1))*toNumber(eval(env,e2)))

      /* Binary Comparison Ops */
      // return first to eval to false or if both are true return the first expr
      case And => //if(!toBoolean(eval(env,e1))) eval(env,e1) else eval(env,e2)
                  val evale1 = eval(env,e1)
        if(!toBoolean(evale1)) evale1 else eval(env,e2)
      // return the first to eval to true, if both false return the 2nd expr
      case Or => if(toBoolean(eval(env,e1))) eval(env,e1) else eval(env,e2)
      case Eq => (eval(env,e1),eval(env,e2)) match {
        //case (S(s1), S(s2)) => B(s1 == s2)
        //case (Undefined, Undefined) => B(true)
        case (expr1,expr2) => B(if(expr1 == expr2) true else false)
      }
      case Ne => (eval(env,e1),eval(env,e2)) match {
        //case (S(s1), S(s2)) => B(s1 != s2)
        //case (Undefined, Undefined) => B(false)
        case (expr1,expr2) => B(if(expr1 != expr2) true else false)
      }
      case Lt => (eval(env,e1),eval(env,e2)) match {
        case (S(s1), S(s2)) => B(s1 < s2)
        case (expr1, expr2) => B(if(toNumber(expr1) < toNumber(expr2)) true else false)
      }
      case Le => (eval(env,e1),eval(env,e2)) match {
        case (S(s1), S(s2)) => B(s1 <= s2)
        case (expr1, expr2) => B(if(toNumber(expr1) <= toNumber(expr2)) true else false)
      }
      case Gt => (eval(env,e1),eval(env,e2)) match {
        case (S(s1), S(s2)) => B(s1 > s2)
        case (expr1, expr2) => B(if(toNumber(expr1) > toNumber(expr2)) true else false)
      }
      case Ge => (eval(env,e1),eval(env,e2)) match {
        case (S(s1), S(s2)) => B(s1 >= s2)
        case (expr1, expr2) => B(if(toNumber(expr1) >= toNumber(expr2)) true else false)
      }

      /* Sequence Op */
      case Seq => eval(env, e1); eval(env,e2)
    }
    /* Ternary Op*/
    case If(e1, e2, e3) => if(toBoolean(eval(env,e1))) {eval(env, e2)} else {eval(env, e3)}
     // if e1 evals to true eval e2 else eval e3

    /* Unary */
    case Unary(uop, e1) => uop match{
        case Neg => eval(env,e1) match {
          case N(0.0) => N(-0.0)
          case _ => N(-toNumber(eval(env,e1)))
        }
        case Not => B(!toBoolean(eval(env,e1)))
      }

    /* Var */
    //case Var(x) => try {lookup(env, x)} catch { case e:java.util.NoSuchElementException => Undefined}

    /* ConstDecl */
    case ConstDecl(x, e1, e2) => eval(extend(env , x, eval(env,e1)), e2)

    case _ => e
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
