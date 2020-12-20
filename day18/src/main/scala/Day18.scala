import silex._
import scallion._

object Day18 {
  def interpret(expr: TreeModule.Expr): Long = {
    expr match {
      case TreeModule.LongLiteral(value) => value
      case TreeModule.Plus(lhs, rhs) => interpret(lhs) + interpret(rhs)
      case TreeModule.Times(lhs, rhs) => interpret(lhs) * interpret(rhs)
    }
  }

  def solvePart(part: Int, inputs: List[String]): String = {
    inputs.map(MyLexer.run)
      .map(new Parser(part).run)
      .map(interpret)
      .sum
      .toString
  }

  def solve(inputs: List[String]): String = {
    solvePart(1, inputs)
  }

  def solve2(inputs: List[String]): String = {
    solvePart(2, inputs)
  }



  sealed trait Token extends Product

  object Tokens {
    final case class LongLitToken(value: Long) extends Token      // e.g. integer literal "123"
    final case class DelimiterToken(value: String) extends Token  // ()
    final case class OperatorToken(name: String) extends Token    // "+" "*"
    final case class SpaceToken() extends Token                   // " "
    final case class EOFToken() extends Token                     // special token at the end of file
  }

  sealed abstract class TokenKind

  object TokenKinds {
    final case object LiteralKind extends TokenKind
    final case class DelimiterKind(value: String) extends TokenKind
    final case class OperatorKind(value: String) extends TokenKind
    final case object EOFKind extends TokenKind
    final case object NoKind extends TokenKind
  }

  object TokenKind {
    import TokenKinds._
    import Tokens._

    def of(token: Token): TokenKind = token match {
      case LongLitToken(_) => LiteralKind
      case DelimiterToken(value) => DelimiterKind(value)
      case OperatorToken(value) => OperatorKind(value)
      case EOFToken() => EOFKind
      case _ => NoKind
    }
  }
  // The lexer for Amy.
  object MyLexer extends Lexers {
    type Character = Char
    type Token = Day18.Token
    type Position = StringPosition

    import Tokens._

    val lexer = Lexer(
      oneOf("+*") |> { (cs, _) => OperatorToken(cs.mkString) },
      many1(elem(_.isDigit)) |> { (cs, _) => LongLitToken(cs.mkString.toLong)},
      oneOf("()") |> { (cs, _) => DelimiterToken(cs.mkString) },
      many1(elem(_.isWhitespace)) |> { (_, _) => SpaceToken() }
    ) onEnd {
      _ => EOFToken()
    }

    def run(source: String): Iterator[Token] = {
      var it = Seq[Token]().iterator

      it ++= lexer.spawn(Source.fromString(source)).filter {
        case SpaceToken() => false
        case _ => true
      }

      it
    }
  }

  object TreeModule {
    // Common ancestor for all trees
    trait Tree

    // Expressions
    trait Expr extends Tree

    // Literals
    case class LongLiteral(value: Long) extends Expr

    // Binary operators
    case class Plus(lhs: Expr, rhs: Expr) extends Expr
    case class Times(lhs: Expr, rhs: Expr) extends Expr
  }

  // The parser for Amy
  class Parser(part: Int) extends Parsers {

    import TokenKinds._
    import Tokens._
    import TreeModule._

    type Token = Day18.Token
    type Kind = Day18.TokenKind

    import Implicits._

    override def getKind(token: Token): TokenKind = TokenKind.of(token)

    val eof: Syntax[Token] = elem(EOFKind)
    def op(string: String): Syntax[String] = accept(OperatorKind(string)) { case OperatorToken(name) => name }

    implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

    lazy val line: Syntax[Expr] = expr ~<~ eof

    lazy val expr: Syntax[Expr] = recursive { operation }

    // Using `operators` to take care of associativity and precedence
    lazy val operation: Syntax[Expr] =
      // Defines the different operators, by decreasing priority.
      if (part == 1) {
        operators(simpleExpr)(
          op("+") | op("*") is LeftAssociative
        ){
          case (lhs, "*", rhs) => Times(lhs, rhs)
          case (lhs, "+", rhs) => Plus(lhs, rhs)
        }
      } else if (part == 2) {
        operators(simpleExpr)(
          op("+") is LeftAssociative,
          op("*") is LeftAssociative
        ){
          case (lhs, "*", rhs) => Times(lhs, rhs)
          case (lhs, "+", rhs) => Plus(lhs, rhs)
        }
      } else {
        operators(simpleExpr)(
          op("*") is LeftAssociative,
          op("+") is LeftAssociative
        ){
          case (lhs, "*", rhs) => Times(lhs, rhs)
          case (lhs, "+", rhs) => Plus(lhs, rhs)
        }
      }

    lazy val simpleExpr: Syntax[Expr] = longLiteral.up[Expr] | parenthesis

    // A literal expression.
    lazy val longLiteral: Syntax[LongLiteral] = accept(LiteralKind) {
      case LongLitToken(value) => LongLiteral(value)
    }

    lazy val parenthesis: Syntax[Expr] = ("(".skip ~ expr ~ ")".skip)

    def run(tokens: Iterator[Token]): Expr = {
      val parser = Parser(line)

      parser(tokens) match {
        case Parsed(result, rest) => result
        case UnexpectedEnd(rest) => println("Unexpected end of input."); LongLiteral(0)
        case UnexpectedToken(token, rest) => println("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", ")); LongLiteral(0)
      }
    }
  }
}
