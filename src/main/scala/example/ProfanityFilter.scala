package example

import java.util.Locale

import scala.util.matching.Regex

/*
  TODO - tail recursion
  TODO - modify to work for code points, not chars (??)
 */

/*
  Constructs a single regex designed to match any uses of the supplied profane words & locale. Matches against the words
  are case insensitive and are designed to match any variants of each word that could be derived by single letter
  substitution. For example, `sh!t` will be considered profane as it is a variant that matches `shit`.

  The technique for creating the regex is as follows. First ensure that each profane word is in lowercase. Partition
  each profane word into its constituent characters. Create a regex of the form: .*("a"|"b"|"c"|...).* formed by combining
  the character with any of its equivalence substitutions. Ensure that any characters that have special meaning in a
  regular expression are escaped. This regex has a leading & trailing `.*` expression to account for any embedded uses.
  Recursively combine this regex with the regex created from the rest of the characters,
  i.e.: .*("a"|"b"|"c"|...).*(<rest>). Finally, combine the resulting regexes for each word using the `|` operator.

  Note that this accounts for single letter substitutions only.
  Note that this works only for the BMP (Basic Multilingual Plane) since code points are not considered.
 */

object ProfanityFilter {

  val locale = Locale.getDefault()

  // Ensure lowercase
  private val profaneWords = List(
    "pussy",
    "cunt",
    "shit",
    "fuck",
    "cocksucker",
    "motherfucker",
    "tits",
    "labia"
  ).map(_.toLowerCase(locale))

  // Single letter substitutions. Ensure keys and values are lowercase.
  private val subs = Map[Char, List[String]](
    's' -> List("5", "$"),
    'u' -> List("v"),
    'i' -> List("!", "1", "|"),
    'c' -> List("k"),
    'k' -> List("c"),
    'f' -> List("Ph"),
    'a' -> List("@"),
    'l' -> List("\\")
  ).map { case (k, v) => (k.toLower, v.map(_.toLowerCase(locale))) }

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

  private def escape(s: String): String = s.toList.map(c => specialChars.getOrElse(c, c.toString)).mkString

  // Returns a regex for the single char of the form (a|b|c) where a,b,& c are strings
  // `c` is guaranteed to be lower case when this is called
  private def charRegex(c: Char): String = {
    // The regex for a single code point is itself plus any substitutions. Matches are case insensitive.
    val strings = c.toString :: subs.getOrElse(c, List.empty)
    val escaped = strings.map(escape)

    escaped.mkString("(", "|", ")")
  }

  // Returns a regex for the passed word that accounts for any embedded uses.
  // The passed word is guaranteed to be in lowercase when this is called.
  private def wordRegex(word: String): Regex = {
    def _wordRegex(chars: List[Char]): String = chars match {
      case Nil => ""
      case head :: Nil => charRegex(head)
      case head :: tail => charRegex(head) + "(" + _wordRegex(tail) + ")"
    }

    // Construct regex to account for embedded uses
    (".*" + _wordRegex(word.toList) + ".*").r
  }

  // The mondo profanity regex
  val profanityRegex: Regex = {
    val rs = profaneWords.map(wordRegex)
    rs.mkString("|").r
  }

  // Checks the passed list of words against the filter and returns the passed word with a Boolean flag: true if it is
  // profane, false if not.
  def profanityCheck(words: List[String]): List[(String, Boolean)] = {
    words.map(w => w.toLowerCase(locale) match {
      case profanityRegex(_*) => (w, true)
      case _ => (w, false)
    })
  }

  // Test
  def main(args: Array[String]): Unit = {
    val profane = List("em-pu$$y-bedded", "cvnt", "kunt", "@pple", "phuck", "Phuck")

    println(profanityRegex)

    val result = profanityCheck(profane)

    println(result)

  }

}
