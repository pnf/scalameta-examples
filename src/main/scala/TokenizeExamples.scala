import scala.meta._
import scala.meta.tokens.Token.{Ident, KwNull, LeftParen}

case class TokenizeExamples(tokens: Tokens) {
  type Index = Int
  val getOrElseFlag = "getOrElse"
  val nullFlag      = "null"
  val leftParenFlag = "leftParen"

  def replaceGetOrElseNull = {
    val indexedTokens = tokens.zipWithIndex.toList
    checkGetOrElse(indexedTokens)
  }

  private def checkGetOrElse(indexedTokens: List[(Token, Index)]): Tokens =
    check(indexedTokens, getOrElseFlag)

  private def checkLeftParenthesis(indexedTokens: List[(Token, Index)]): Tokens =
    check(indexedTokens, leftParenFlag)

  private def isNull(indexedTokens: List[(Token, Index)]): Tokens  =
    check(indexedTokens, nullFlag)

  private def check(indexedTokens: List[(Token, Index)], expected: String): Tokens = {
    indexedTokens match {
      case (Ident("getOrElse"), _)   :: xs if expected == getOrElseFlag => checkLeftParenthesis(xs)
      case (LeftParen(), _)          :: xs if expected == leftParenFlag => isNull(xs)
      case (KwNull(), nullIndex)     :: xs if expected == nullFlag      => replaceTokens(nullIndex)
      case x :: Nil => tokens
      case Nil      => tokens
      case _        => checkGetOrElse(indexedTokens.tail)
    }
  }

  private def replaceTokens(nullIndex: Index): Tokens = {
    val nullAndRightParenOffset = 2
    val (beforeNull, afterNull) = tokens.splitAt(nullIndex)
    val replaced = beforeNull.reverse.tail.tail.reverse.mkString + "orNull"
    val rest = afterNull.drop(nullAndRightParenOffset).mkString

    val newCode = replaced.tokenize.get.toString() + TokenizeExamples(rest.tokenize.get).replaceGetOrElseNull.toString
    newCode.tokenize.get
  }
}
