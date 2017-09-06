package example

import scala.util.matching.Regex

/*
  TODO - tail recursion
  TODO - modify to work for code points, not chars
 */


/*
  Accounts for single letter substitutions only. The list of profanity words are stored in lowercase.
 */
object ProfanityFilter {

  // MUST be in all lowercase
  private val profaneWords = List(
    "pussy",
    "cunt",
    "shit",
    "fuck",
    "cocksucker",
    "motherfucker",
    "tits"
  )

  // Single letter substitutions. Keys MUST be lowercase since the list of profane words are lowercase.
  private val subs = Map[Char, List[String]](
    's' -> List("5", "$"),
    'u' -> List("v"),
    'i' -> List("!", "1", "|"),
    'c' -> List("k"),
    'k' -> List("c"),
    'f' -> List("Ph"),
    'a' -> List("@")
  )

  /*
    These must be escaped:
      12 characters with special meanings: the backslash \, the caret ^, the dollar sign $, the period or dot .,
      the vertical bar or pipe symbol |, the question mark ?, the asterisk or star *, the plus sign +,
      the opening parenthesis (, the closing parenthesis ), the opening square bracket [, and the opening curly brace {
   */
  private val specialChars = Map[Char, String](
    '\\' -> """\\""",
    '^' -> """\^""",
    '$' -> """\$""",
    '.' -> """\.""",
    '|' -> """\|""",
    '?' -> """\?""",
    '*' -> """\*""",
    '+' -> """\+""",
    '(' -> """\(""",
    ')' -> """\)""",
    '[' -> """\[""",
    '{' -> """\{"""
  )
  private def escape(s: String): String = {
    val l = s.toList.map(c => specialChars.getOrElse(c, c.toString))
    l.reduce(_ ++ _)
  }

  // Returns a regex for the single char of the form (a|b|c) where a,b,& c are strings
  // `c` is guaranteed to be lower case when this is called
  private def charRegex(c: Char): String = {
    val strings = c.toString :: subs.getOrElse(c, List.empty).map(_.toLowerCase)
    val escaped = strings.map(escape)

    escaped.mkString("(", "|", ")")
  }

  private def wordRegex(word: String): Regex = {
    def _wordRegex(chars: List[Char]): String = chars match {
      case Nil => ""
      case head :: Nil => charRegex(head)
      case head :: tail => charRegex(head) + "(" + _wordRegex(tail) + ")"
    }
    (".*" + _wordRegex(word.toLowerCase.toList) + ".*").r
  }

  val profanityRegex: Regex = {
    val rs = profaneWords.map(wordRegex)
    rs.mkString("|").r
  }

  def profanityCheck(words: List[String]): List[(String, Boolean)] = {
    words.map(w => w.toLowerCase match {
      case profanityRegex(_*) => (w, true)
      case _ => (w, false)
    })
  }

  def main(args: Array[String]): Unit = {
    val profane = List("em-pu$$y-bedded", "cvnt", "kunt", "@pple", "phuck", "Phuck")

    println(profanityRegex)

    val result = profanityCheck(profane)

    println(result)

  }

}
