/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Token
 * Student(s) Name(s):
 */

// TODO: update this enumeration with the token possible values
object Token extends Enumeration {
  val EOF             = Value
  val IDENTIFIER      = Value
  val PROGRAM         = Value
  val VAR             = Value


  val BOOLEAN         = Value
  val TRUE            = Value
  val FALSE           = Value
  val INTEGER         = Value


  val END             = Value


  val OPERATOR        = Value
  val ADD_OP          = Value
  val SUB_OP          = Value
  val MUL_OP          = Value
  val DIV_OP          = Value
  val GRE_OP          = Value
  val LES_OP          = Value
  val EQU             = Value


  val SEMI_COL        = Value
  val COL             = Value
  val PERIOD          = Value

  val INT_LITERAL     = Value
  val DIGIT           = Value
  val OPEN_BRACE      = Value
  val CLOSE_BRACE     = Value
  val OPEN_PAR        = Value
  val CLOSE_PAR       = Value
  val META_IDENT      = Value
  val TERMINAL_STRING = Value


  val ELSE            = Value
  val THEN            = Value

  val DO              = Value

  val IF              = Value
  val READ            = Value
  val WRITE           = Value
  val WHILE           = Value
  val ASSIGN_STMT     = Value
  val BEGIN           = Value
}