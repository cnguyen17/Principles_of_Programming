/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Syntax Analyzer
 * Student(s) Name(s): Calvin Nguyen and Osman Rakhimov
 */

class SyntaxAnalyzer(private var source: String) {

  private var it = new LexicalAnalyzer(source).iterator
  private var lexemeUnit: LexemeUnit = null

  private def getLexemeUnit() = {
    if (lexemeUnit == null)
      lexemeUnit = it.next()
  }

  def parse(): Tree = {
    parseProgram()
  }

  // TODO: finish the syntax analyzer

  // program = `program` identifier body `.`
  private def parseProgram() = {
    // create a tree with label "program"
    val tree = new Tree("program")
    getLexemeUnit()
    if (lexemeUnit.getToken() == Token.PROGRAM) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      if (lexemeUnit.getToken() == Token.VAR){
        throw new Exception("Syntax Analyzer Error: Identifier expected!")
      }
      tree.add(parseIdentifier())
      tree.add(parseBody)
        if (lexemeUnit.getToken() == Token.PERIOD) {
              tree.add(new Tree(lexemeUnit.getLexeme()))
              lexemeUnit = null
              getLexemeUnit()
          if (lexemeUnit.getToken() == Token.IDENTIFIER){
            throw new Exception ("Syntax Analyzer Error: EOF expected!")

          }
            }
            else
              throw new Exception("Syntax Analyzer Error: period expected!")
    }

      else
      throw new Exception("Syntax Analyzer Error: program expected!")
    // return the tree
    tree
  }

  // identifier = letter { ( letter | digit ) }
  // TODO: return a new tree with the label "identifier" followed by the actual lexeme
  private def parseIdentifier() = new Tree("identifier: '" + lexemeUnit.getLexeme() + "'")

  // body = [ var_sct ] block
  private def parseBody() = {
    // create a tree with label "program"
    val tree = new Tree("body")
    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parse_varSct())
      tree.add(parseBlock())
    }
    else
      throw new Exception("Syntax Analyzer Error: body expected!")
    // return the tree
    tree
  }


  // var_sct = ´var´ var_dcl { ´;´ var_dcl }
  private def parse_varSct(): Tree = {
    //    println("parseDefinitionList")
    val tree = new Tree("var_sct")
    lexemeUnit = null
    getLexemeUnit()
    if (lexemeUnit.getToken() != Token.VAR){
      throw new Exception ("Syntax Analyzer Error: begin expected!")
    }
    else if (lexemeUnit.getToken() == Token.VAR) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    }

    var done = false
    while (!done) {
      tree.add(parse_valDlc())
      if (lexemeUnit.getToken() == Token.SEMI_COL) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
      else
        done = true

    }
    tree
  }

  // var_dcl = identifier { identifier } ´:´ type
  private def parse_valDlc(): Tree = {
    //    println("parseSingleDefinition")
    val tree = new Tree("var_dct")
    getLexemeUnit()

    var done = false
    while (!done) {

      //      println(lexemeUnit)
      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseIdentifier())
        lexemeUnit = null
        getLexemeUnit()
        if (lexemeUnit.getToken() == Token.INTEGER || lexemeUnit.getToken() == Token.BOOLEAN){
          throw new Exception ("Syntax Analyzer Error: colon expected!")
        }
      }
      else if (lexemeUnit.getToken() == Token.COL) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
      else
        done = true

    }

    tree.add(parseType())
    lexemeUnit = null
    getLexemeUnit()
    tree
  }


  // type = ´Integer´ | ´Boolean´
  private def parseType(): Tree = {
    // TODO: create a tree with label "term'"
    val tree = new Tree("type")

    // TODO: call getLexemeUnit
    getLexemeUnit()

    // TODO: if token is NOT EOF
    if (lexemeUnit.getToken() != Token.EOF) {

      // TODO: if token is "*" or "/", add token as new branch and reset lexemeUnit;
      //  then add result of "parseFactor" and "parseTermPrime" as new branches
      if (lexemeUnit.getToken() == Token.INTEGER || lexemeUnit.getToken() == Token.BOOLEAN) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null // always set lexemeUnit to null after consuming it
      }
      else
        throw new Exception("Syntax Analyzer Error: type expected!")
    }
    tree
  }


  // block = ´begin´ stmt { ´;´ stmt } ´end´
  private def parseBlock(): Tree = {
    //    println("parseDefinitionList")
    val tree = new Tree("block")
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.BEGIN) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    }

    var done = false
    while (!done) {
      tree.add(parseStmt())
      if (lexemeUnit.getToken() == Token.SEMI_COL) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
      else
        done = true
    }
    if (lexemeUnit.getToken() == Token.END) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    }
    else {
      throw new Exception("Syntax Analyzer Error: end expected!")
    }
    tree
  }

  // stmt = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block
  private def parseStmt(): Tree = {
    val tree = new Tree("stmt")
    getLexemeUnit()
    if (lexemeUnit.getToken() == Token.IDENTIFIER)
      tree.add(parse_assgmStmt())
    else if (lexemeUnit.getToken() == Token.READ)
      tree.add(parseread_Stmt())
    else if (lexemeUnit.getToken() == Token.WRITE)
      tree.add(parsewrite_Stmt())
    else if (lexemeUnit.getToken() == Token.IF)
      tree.add(parse_if_Stmt())
    else if (lexemeUnit.getToken() == Token.WHILE)
      tree.add(parseWhile_Stmt())
    else if (lexemeUnit.getToken() == Token.BEGIN)
      tree.add(parseBlock())
    tree
  }

  // assgm_stmt = identifier ´:=´ expr
  private def parse_assgmStmt(): Tree = {
    val tree = new Tree("assgm_stmt")
    getLexemeUnit()
    if (lexemeUnit.getToken() == Token.IDENTIFIER) {

      tree.add(parseIdentifier())
      lexemeUnit = null
      getLexemeUnit()
      if (lexemeUnit.getToken() == Token.ASSIGN_STMT) { //Be more specific?
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseExpr())
      }
      else
        throw new Exception("Syntax Analyzer Error: assignment expected!")
    }
    else
      throw new Exception("Syntax Analyzer Error: identifier expected!")
    tree
  }

  // read_stmt = ´read´ identifier
  private def parseread_Stmt() = {
    // create a tree with label "program"
    val tree = new Tree("read_stmt")
    getLexemeUnit()
    if (lexemeUnit.getToken() == Token.READ) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parseIdentifier())
      lexemeUnit = null
      getLexemeUnit()
    }
    else
      throw new Exception("Syntax Analyzer Error: program expected!")
    // return the tree
    tree
  }

  // write_stmt = ´write´ ( identifier | literal )
  private def parsewrite_Stmt(): Tree = {
    // TODO: create a tree with label "write_stmt"
    val tree = new Tree("write_stmt")

    // TODO: call getLexemeUnit
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.WRITE) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      if (lexemeUnit.getToken() == Token.IDENTIFIER||lexemeUnit.getToken() == Token.TRUE || lexemeUnit.getToken() == Token.FALSE){
        tree.add(parseIdentifier())
        lexemeUnit = null
        getLexemeUnit()

      }

    }
      else {
        throw new Exception("Syntax Analyzer Error: identifier, literal or \"opening parenthesis\" expected!")
    }

    // TODO: return the tree
    tree
  }
  // if_stmt = ´if´ bool_expr ´then´ stmt [ ´else´ stmt ]
  private def parse_if_Stmt(): Tree = {
    //    println("parseOptionalSequence")
    val tree = new Tree("if_stmnt")
    getLexemeUnit()
    if (lexemeUnit.getToken() == Token.IF) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parseBool_Expr())
      if (lexemeUnit.getToken() == Token.THEN) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseStmt())
        if (lexemeUnit.getToken() == Token.ELSE) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          getLexemeUnit()
          tree.add(parseStmt())
        }
      }
        else{
          throw new Exception("Syntax Analyzer Error: then expected!")}
    }
    else
      throw new Exception("Syntax Analyzer Error: \"if\" expected!")
    tree
  }

  // while_stmt = ´while´ bool_expr ´do´ stmt
  private def parseWhile_Stmt(): Tree = {
    //    println("parseOptionalSequence")
    val tree = new Tree("while_stmnt")
    getLexemeUnit()
    if (lexemeUnit.getToken() == Token.WHILE) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parseBool_Expr())

      if (lexemeUnit.getToken() == Token.DO) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseStmt())
      }

      else
        throw new Exception("Syntax Analyzer Error: do expected!")
    }
    else
      throw new Exception("Syntax Analyzer Error: \"if\" expected!")
    tree
  }



  // expr = arithm_expr | bool_expr
  private def parseExpr(): Tree = {
    // TODO: create a tree with label "expr"
    val tree = new Tree("expr")

    // TODO: call getLexemeUnit
    getLexemeUnit()

    // TODO: if token is NOT EOF
    if (lexemeUnit.getToken() != Token.EOF) {
      // TODO: if token is an identifier or Int_literal, add result of "parse_arithm_Expr" as new branch and reset lexemeUnit
      if (lexemeUnit.getToken() == Token.IDENTIFIER ||lexemeUnit.getToken() == Token.INT_LITERAL) {
        tree.add(parse_arithm_Expr())
//        lexemeUnit = null // always set lexemeUnit to null after consuming it
      }
      // TODO: if token is a bool_expre (True or False), add result of "parseBool_Expr" as new branch and reset lexemeUnit
      else if (lexemeUnit.getToken() == Token.TRUE ||lexemeUnit.getToken() == Token.FALSE) {
        tree.add(parseBool_Expr())
        lexemeUnit = null // always set lexemeUnit to null after consuming it
        getLexemeUnit()
      }
      // TODO: otherwise, throw an exception saying that "identifier, literal or opening parenthesis" was expected
      else
        throw new Exception("Syntax Analyzer Error: identifier or (int) literal expected!")
    }
    // TODO: otherwise, throw an exception saying that "identifier, literal or opening parenthesis" was expected
    else
      throw new Exception("Syntax Analyzer Error: identifier or (int) literal expected!")

    // TODO: return the tree
    tree
  }

  // arithm_expr = term arithm_expr'
  private def parse_arithm_Expr() = {
    // TODO: create a tree with label "arithm_expr"
    val tree = new Tree("arithm_expr")

    // TODO: call getLexemeUnit
    getLexemeUnit()

    // TODO: if token is NOT EOF, add result of "parseTerm" and "parse_arithm_expr_Prime" as new branches
    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parseTerm())
      tree.add(parse_arithm_expr_Prime())
    }

    // TODO: otherwise, throw an exception saying that "factor" was expected
    else
      throw new Exception("Syntax Analyzer Error: factor expected!")

    // TODO: return the tree
    tree
  }

  //arith_expr'= ( ´+´ | ´-´ ) term arithm_expr'| epsilon
  private def parse_arithm_expr_Prime(): Tree = {
    // TODO: create a tree with label "arith_expr'"
    val tree = new Tree("arith_expr'")
    // TODO: While loop and call getLexemeUnit

    var done = false
    while (!done){
      getLexemeUnit()
      // TODO: if token is NOT EOF
      if (lexemeUnit.getToken() != Token.EOF) {

      // TODO: if token is "+" or "-", add token as new branch and reset lexemeUnit;
      //  then add result of "parseTerm" and "parse_arithm_expr_Prime" as new branches
      if (lexemeUnit.getToken() == Token.ADD_OP || lexemeUnit.getToken() == Token.SUB_OP) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null // always set lexemeUnit to null after consuming it
        tree.add(parseTerm())
        tree.add(parse_arithm_expr_Prime())

      }
      // else means "epsilon" production
      else {
        done = true
      }
      }
    }
    // TODO: return the tree
    tree
  }

  // term = factor term'

  private def parseTerm() = {
    // TODO: create a tree with label "term"
    val tree = new Tree("term")

    // TODO: call getLexemeUnit
    getLexemeUnit()

    // TODO: if token is NOT EOF, add result of "parseFactor" and "parseTermPrime" as new branches
    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parseFactor())
      tree.add(parseTermPrime())
    }
    // TODO: otherwise, throw an exception saying that "factor" was expected
    else
      throw new Exception("Syntax Analyzer Error: factor expected!")

    // TODO: return the tree
    tree
  }

  // term' = '*'  factor term' | epsilon
  private def parseTermPrime(): Tree = {
    // TODO: create a tree with label "term'"
    val tree = new Tree("term'")

    // TODO: call getLexemeUnit
    getLexemeUnit()

    // TODO: if token is NOT EOF
    if (lexemeUnit.getToken() != Token.EOF) {

      // TODO: if token is "*", add token as new branch and reset lexemeUnit;
      //  then add result of "parseFactor" and "parseTermPrime" as new branches
      if (lexemeUnit.getToken() == Token.MUL_OP) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        tree.add(parseFactor())
        tree.add(parseTermPrime())

      }
      // else means "epsilon" production
    }

    // TODO: return the tree
    tree
  }


  // factor = identifier | int_literal
  private def parseFactor(): Tree = {
    // TODO: create a tree with label "factor"
    val tree = new Tree("factor")

    // TODO: call getLexemeUnit
    getLexemeUnit()

    // TODO: if token is NOT EOF
    if (lexemeUnit.getToken() != Token.EOF) {
      // TODO: if token is an identifier, add result of "parseIdentifier" as new branch and reset lexemeUnit
      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseIdentifier())
        lexemeUnit = null // always set lexemeUnit to null after consuming it
      }
      // TODO: if token is a Int_literal, add result of "parse_intLiteral" as new branch and reset lexemeUnit
      else if (lexemeUnit.getToken() == Token.INT_LITERAL) {
        tree.add(parse_intLiteral())
        lexemeUnit = null // always set lexemeUnit to null after consuming it
      }

      // TODO: otherwise, throw an exception saying that "identifier, literal or opening parenthesis" was expected
      else
        throw new Exception("Syntax Analyzer Error: identifier or \"(int) literal\" expected!")
    }
    // TODO: otherwise, throw an exception saying that "identifier, literal or opening parenthesis" was expected
    else
      throw new Exception("Syntax Analyzer Error: identifier or \"(int) literal\" expected!")

    // TODO: return the tree
    tree
  }


  // literal = int_literal | bool_literal
  private def parseLiteral(): Tree = {
    // TODO: create a tree with label "literal"
    val tree = new Tree("literal")

    // TODO: call getLexemeUnit
    getLexemeUnit()

    // TODO: if token is NOT EOF
    if (lexemeUnit.getToken() != Token.EOF) {
      // TODO: if token is a Int_literal, add result of "parseinyLiteral" as new branch and reset lexemeUnit
      if (lexemeUnit.getToken() == Token.INT_LITERAL) {
        tree.add(parse_intLiteral())
        lexemeUnit = null // always set lexemeUnit to null after consuming it
      }
      // TODO: if token is a Int_boolean, add result of "parse_bool_Literal" as new branch and reset lexemeUnit
      else if (lexemeUnit.getToken() == Token.TRUE || lexemeUnit.getToken() == Token.FALSE) {
        tree.add(parse_bool_literal())
        lexemeUnit = null // always set lexemeUnit to null after consuming it
      }

      // TODO: otherwise, throw an exception saying that "identifier, literal or opening parenthesis" was expected
      else
        throw new Exception("Syntax Analyzer Error: \"(int) literal\" or \"(bool) literal\" expected!")
    }
    // TODO: otherwise, throw an exception saying that "identifier, literal or opening parenthesis" was expected
    else
      throw new Exception("Syntax Analyzer Error: \"(int) literal\" or \"(bool) literal\" expected!")

    // TODO: return the tree
    tree
  }


  // int_literal = digit { digit }
  // TODO: return a new tree with the label "literal" followed by the actual lexeme
  private def parse_intLiteral() = new Tree("int_literal: '" + lexemeUnit.getLexeme() + "'")


  // bool_literal = ´true´ | ´false´
  private def parse_bool_literal(): Tree = {
    // TODO: create a tree with label "expression'"
    val tree = new Tree("bool_literal")

    // TODO: call getLexemeUnit
    getLexemeUnit()

    // TODO: if token is NOT EOF
    if (lexemeUnit.getToken() != Token.EOF) {
      // TODO: if token is "TRUE" or "FALSE", add token as new branch and reset lexemeUnit;
      //  then add result of "parseTerm" and "parseExpressionPrime" as new branches
      if (lexemeUnit.getToken() == Token.TRUE || lexemeUnit.getToken() == Token.FALSE) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
//        lexemeUnit = null // always set lexemeUnit to null after consuming it

      }
      // else means "epsilon" production
    }

    // TODO: return the tree
    tree
  }
  // bool_expr = bool_literal | arithm_expr ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ ) arithm_exp
  private def parseBool_Expr() = {
    // create a tree with label "program"
    val tree = new Tree("bool_expr")
    getLexemeUnit()

    if(lexemeUnit.getToken() == Token.IDENTIFIER) {
      tree.add(parse_arithm_Expr())
      getLexemeUnit()
      if(lexemeUnit.getToken() == Token.OPERATOR || lexemeUnit.getToken() == Token.EQU
        ||lexemeUnit.getToken() == Token.LES_OP || lexemeUnit.getToken() == Token.GRE_OP ){
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parse_arithm_Expr())
      }
      else{
        throw new Exception ("Syntax Analyzer Error: relational operator expected!")
      }


    }
    if(lexemeUnit.getToken() == Token.TRUE || lexemeUnit.getToken() == Token.FALSE){
         tree.add(parse_bool_literal())

    }
    // return the tree
    tree
  }


}

object SyntaxAnalyzer {
  def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse()
    print(parseTree)

  }
}