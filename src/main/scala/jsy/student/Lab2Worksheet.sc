/*
 * CSCI 3155: Lab 2 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab2.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab2.Parser.parse

// Imports the ast nodes
import jsy.lab2.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab2._

// Call the JavaScripty parser (from the provided library) on a string
val negFourAST = parse("-4")

// Evaluate that JavaScripty expression.
//eval(negFourAST)

// For convenience, we also have an eval function that takes a string,
// which calls the parser and then delegates to your eval function.
//eval("undefined + 1")

//parse("NaN || false || 123 ")
//eval("NaN || false || 123 ")
eval("!!0")

parse("false - 3 -4")
toStr(N(Double.NaN))
//toStr(N(100/0))

toStr(N(1.0/0.0))
eval("\"\" + true + 1")
1.0/0.0
eval("5 && false")
//eval(Binary(Lt, S("ab"), N(Double.NaN)))
eval("(undefined < 1)*10")
eval("-0 && 0")
false == false
eval("console.log(3) + console.log(31)")
eval("1, 2, \"1,1\"")
eval("true + 1 + \"hi\"")
eval("-11230 + \"hs\"")
eval("\"aaa\" < \"bb\"")
eval("\"asdf\" && true")
eval("\"\" && true")
eval("3 / \"asd\"")
eval("\"\"?1:2")
eval("1 && 2 * 2 || 1")
eval("\"9\" - 9")
eval("\"\" - 9")
eval("false !== 0")
eval("-\"\"")
eval("0/0 + \"asf\"")
eval("!(0/0)")
eval(N(-0.0))
N(Double.NaN) != N(Double.NaN)
eval("1 + \"0/0\"")
eval("undefined < 1")









































