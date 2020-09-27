import LexicalAnalyzer.{WORD_TO_TOKEN, _}

import scala.io.Source

/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Lexical Analyzer
 * Student(s) Name(s): Calvin Nguyen and Osman
 */

class LexicalAnalyzer(private var source: String) extends Iterable[LexemeUnit] {

  private var input = ""
  for (line <- Source.fromFile(source).getLines)
    input += line + "\n"

  // determines the class of a given character
  private def getCharClass(c: Char): CharClass.Value = {
    if (LETTERS.contains(c))
      CharClass.LETTER
    else if (DIGITS.contains(c))
      CharClass.DIGIT
    else if (BLANKS.contains(c))
      CharClass.BLANK
    else if (c == '+' || c == '-' || c == '*' || c == '/' ||  c == '>' || c == '<' || c == '=')
      CharClass.OPERATOR
    else if (c == '.' || c == ',' || c == ';' || c == ':')
      CharClass.PUNCTUATOR
    else if (c == '(' || c == ')')
      CharClass.DELIMITER
    else
      CharClass.OTHER
  }

  // reads the input until a non-blank character is found, returning the input updated
  private def readBlanks: Unit = {
    var foundNonBlank = false
    while (input.length > 0 && !foundNonBlank) {
      val c = input(0)
      if (getCharClass(c) == CharClass.BLANK)
        input = input.substring(1)
      else
        foundNonBlank = true
    }
  }

  def iterator: Iterator[LexemeUnit] = {
    new Iterator[LexemeUnit] {

      override def hasNext: Boolean = {
        readBlanks
        input.length > 0
      }

      override def next(): LexemeUnit = {
        if (!hasNext)
          new LexemeUnit("", Token.EOF)
        else {
          var lexeme = ""
          readBlanks
          if (input.length == 0)
            new LexemeUnit(lexeme, Token.EOF)
          else {
            var c = input(0)
            var charClass = getCharClass(c)

            // TODO: recognize a Letter/ Identifier
            if (charClass == CharClass.LETTER) {
              input = input.substring(1)
              lexeme += c
              var noMoreLetterDigits = false
              while (!noMoreLetterDigits) {
                if (input.length == 0)
                  noMoreLetterDigits = true
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.LETTER || charClass == CharClass.DIGIT) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreLetterDigits = true
                }
              }
              if (WORD_TO_TOKEN.contains(lexeme))
                return new LexemeUnit(lexeme, WORD_TO_TOKEN.get(lexeme).get)
              else
                return new LexemeUnit(lexeme, Token.IDENTIFIER)
            }
            // TODO: recognize a Int_literal
            if (charClass == CharClass.DIGIT) {
              input = input.substring(1)
              lexeme += c
              var noMoreDigits = false
              while (!noMoreDigits) {
                if (input.length == 0)
                  noMoreDigits = true
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.DIGIT) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreDigits = true
                }
              }
              return new LexemeUnit(lexeme, Token.INT_LITERAL)
            }
            // TODO: recognize a operator

            if (charClass == CharClass.OPERATOR) {
              input = input.substring(1)
              lexeme += c
              var noMoreLetterDigits = false
              while (!noMoreLetterDigits) {
                if (input.length == 0)
                  noMoreLetterDigits = true
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.OPERATOR) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreLetterDigits = true
                }
              }
              if (TYPE.contains(lexeme))
                return new LexemeUnit(lexeme, TYPE.get(lexeme).get)
              else
                return new LexemeUnit(lexeme, Token.OPERATOR)
            }
            // TODO: recognize a Punctuator
            if (charClass == CharClass.PUNCTUATOR) {
              input = input.substring(1)
              lexeme += c
              var noMoreLetterDigits = false
              while (!noMoreLetterDigits) {
                if (input.length == 0)
                  noMoreLetterDigits = true
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.PUNCTUATOR || charClass == CharClass.OPERATOR) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreLetterDigits = true
                }
              }
              if (WORD_TO_TOKEN.contains(lexeme))
                return new LexemeUnit(lexeme, WORD_TO_TOKEN.get(lexeme).get)
              else
                return new LexemeUnit(lexeme, Token.ASSIGN_STMT)
            }
               throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")
          }
        }
      } // end next
    } // end 'new' iterator
  } // end iterator method
} // end LexicalAnalyzer class

object LexicalAnalyzer {
  val LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS  = "0123456789"
  val BLANKS  = " \n\t"
  val TYPE = Map(
    "+"  -> Token.ADD_OP,
    "-"  -> Token.SUB_OP,
    "*"  -> Token.MUL_OP,
    "/"  -> Token.DIV_OP,
    "<"  -> Token.GRE_OP,
    ">"  -> Token.LES_OP,
    "="  -> Token.EQU,

    )
  val WORD_TO_TOKEN = Map(
    "program"  -> Token.PROGRAM,
    "var"      -> Token.VAR,
    "Boolean"  -> Token.BOOLEAN,
    "true"     -> Token.TRUE,
    "false"    -> Token.FALSE,
    "Integer"  -> Token.INTEGER,
    "begin"    -> Token.BEGIN,
    "end"      -> Token.END,
    "read"     -> Token.READ,
    "write"    -> Token.WRITE,
    ";"        -> Token.SEMI_COL,
    ":"        -> Token.COL,
    "."        -> Token.PERIOD,
    "if"       -> Token.IF,
    "else"     -> Token.ELSE,
    "then"     -> Token.THEN,
    "while"    -> Token.WHILE,
    "do"       -> Token.DO,
    "\n"       -> Token.BLANK,


  )

  def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val lex = new LexicalAnalyzer(args(0))
    val it = lex.iterator
    while (it.hasNext) {
      val lexemeUnit = it.next()
      println(lexemeUnit)
    }
  } // end main method
} // end LexicalAnalyzer object