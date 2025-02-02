//
// Parser for SimpleC programs. This component checks 
// the input program to see if it meets the syntax rules
// of SimpleC. The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// Modified by:
// Zaheer Safi
//
// Original author:
//   Prof. Joe Hummel
//   University of Illinois Chicago
//   CS 341, Spring 2022
// Modified by:
//   Ellen Kidane
//   University of Illinois Chicago
//   CS 341, Spring 2024
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  // stmt tokens
  //
  // according to BNF a statment can be empty has an assignment
  // input, output or ifstatmen so we check for all of those
  // eventually returning the tokens left
  
  let rec stmt tokens =

    let T1 = empty tokens
    let T2 = vardecl T1
    let T3 = input T2
    let T4 = output T3
    let T5 = assignment T4
    let T6 = ifstmt T5
    
    if (tokens = T6) then
      let next_token = List.head tokens
      failwith("expecting statement, but found " + next_token)
    
    T6

  // ifstmt tokens
  //
  // in BNF the ifstmt should be: <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  // so first we check for "if" and then the open paranthesis "(" and because condition
  // is expr so we cann expr and the call then-part and else-part recursively to verify those
  // parts 
  
  and ifstmt tokens =
    
    let next_token = List.head tokens
    
    if (next_token = "if") then
      
      let ifPart = matchToken "if" tokens
      let paranthesis1 = matchToken "(" ifPart
      let condition = expr paranthesis1
      
      if (paranthesis1 = condition) then
        failwith("expected condition but found " + condition.Head)
      
      let paranthesis2 = matchToken ")" condition
      let then_part = stmt paranthesis2
      
      if (paranthesis2 = then_part) then
        failwith("expected statment but found " + then_part.Head)
      
      else_part then_part
    
    else
      tokens
  
  // else_part tokens
  //
  // in BNF the else_part is <else-part> -> else <stmt> | EMPTY
  // so first we check for the keyword else and the call for <stmt> which can
  // als be empty and eventually return tokens
  
  and private else_part tokens =
    
    let next_token = List.head tokens

    if next_token = "else" then
      let T2 = matchToken "else" tokens 
      stmt T2 
    else
      tokens 
  
  // matchToken expected_token tokens
  //
  // if the token matches the expected token, keep parsing by
  // returning the rest of the tokens. Otherwise throw an
  // exception because there's a syntax error, effectively
  // stopping compilation at the first error.
  // Additinaly we have special cases for identifier and we handle those wiht
  // beginswith and isSpecialToken
  
  and matchToken expected_token tokens =
    
    let next_token = List.head tokens

    let beginswith (pattern: string) (literal: string) =
      literal.StartsWith(pattern)

    let isSpecialToken token expectedType =
        beginswith (expectedType + ":") token

    match expected_token with
    | "identifier" when isSpecialToken next_token "identifier" ->
        List.tail tokens
    | "str_literal" when isSpecialToken next_token "str_literal" ->
        List.tail tokens
    | "int_literal" when isSpecialToken next_token "int_literal" ->
        List.tail tokens
    | "real_literal" when isSpecialToken next_token "real_literal" ->
        List.tail tokens
    | _ when expected_token = next_token ->
        List.tail tokens
    | _ ->
        failwith ("expecting " + expected_token + ", but found " + next_token)


  // empty tokens
  //
  // check for empty if the token is ";" then recursively call the
  // empty token function to check for more semicolons which represent
  // empty in SimpleC
  
  and empty tokens =
    let next_token = List.head tokens
    
    if (next_token = ";") then
      let T1 = matchToken next_token tokens
      empty T1
    else
      tokens
  
  // vardecl tokens
  //
  // vardecl function is used to check if int variable defination is done correctly
  // checking for the int keyword calling identifier with matchtoken and checking for
  // semicolon and eventually return tokens
  
  and vardecl tokens =
    let IntToken = List.head tokens
    
    if (IntToken = "int") then
      let tokensAfterInt = matchToken "int" tokens 
      let tokensAfterIdentifier = matchToken "identifier" tokensAfterInt 
      matchToken ";" tokensAfterIdentifier
    
    else if (IntToken = "real") then
      let tokensAfterReal = matchToken "real" tokens
      let tokensAfterIdentifier = matchToken "identifier" tokensAfterReal
      matchToken ";" tokensAfterIdentifier

    else
      tokens

  // input tokens
  //
  // for input the BNF is <input> -> cin >> identifier;
  // so first we check the "cin" keword and then this symbol ">>"
  // and then call match token with identifer which is a special case handled
  // by matchTokens and eventually return tokens
  
  and input tokens =
    let CinToken = List.head tokens
    
    if (CinToken = "cin") then
      
      let cin = matchToken "cin" tokens
      let symbol = matchToken ">>" cin
      let tokensAfterIdentifier = matchToken "identifier" symbol 
      matchToken ";" tokensAfterIdentifier 

    else
      tokens
  
  
  // expr_value tokens
  //
  // the BNF for expr_value is <expr-value> -> identifier
  // | int_literal
  // | str_literal
  // | true
  // | false
  // so we match token head with one of these and call the matchToken to check its correctness
  // otherwise we return tokens
  
  and expr_value tokens =
    let next_token : string = List.head tokens

    match next_token with
    | _ when next_token.StartsWith("identifier:") -> matchToken "identifier" tokens
    | _ when next_token.StartsWith("int_literal:") -> matchToken "int_literal" tokens
    | _ when next_token.StartsWith("str_literal:") -> matchToken "str_literal" tokens
    | _ when next_token.StartsWith("real_literal:") -> matchToken "real_literal" tokens
    | "true" | "false" -> matchToken next_token tokens
    | _ -> tokens

    
  // output tokens
  //
  // this is the BNF for output : <output> -> cout << <output-value> ;
  // first we check of keyword cout and then the symbol "<<" and then the output value
  // is simple and expr-value so we can expr value but the exp-value should exist and fianally
  // we return the leftover tokens
  
  and output tokens =
    
    let CoutToken = List.head tokens
    
    if (CoutToken = "cout") then
      
      let cout = matchToken "cout" tokens
      let AfterSymbol = matchToken "<<" cout
      let tokensAfterIdentifier = List.head AfterSymbol
      
      if (tokensAfterIdentifier = "endl") then
        let endl = matchToken "endl" AfterSymbol
        matchToken ";" endl 
      
      else
        let T1 =  expr_value AfterSymbol
        
        if (T1 = AfterSymbol) then
          failwith("expecting identifier or literal, but found " + T1.Head)
        
        matchToken ";" T1 
    
    else
      tokens

  // expr_op tokens
  //
  // in expr-op we check for differnt possible operations in SimpleC
  // we take the head of tokens string list and then use the match to to check if it is one 
  // of the operators and then call matchToken on it otherise we return tokens
  
  and expr_op tokens =
    let next_token : string = List.head tokens
    
    match next_token with
    | "+" | "-" | "*" | "/" | "^" | "<" | "<=" | ">" | ">=" | "==" | "!=" -> matchToken next_token tokens
    | _ -> tokens
    
  
  // expr tokens
  //
  // the BNF for expr is : <expr> -> <expr-value> <expr-op> <expr-value> | <expr-value>
  // so either we have expr-value with operator or simply just the expr-value so we check for both
  // on edge case is if there is and expr-opr so the second the expr-value should exist and finally 
  // we return the tokens
  
  and expr tokens =
    
    let tokensAfterExprValue1 = expr_value tokens
    let tokensAfterExprOp = expr_op tokensAfterExprValue1
      
    if (tokensAfterExprValue1 <> tokensAfterExprOp) then
      let tokensAfterExprValue2 = expr_value tokensAfterExprOp
      
      if (tokensAfterExprValue2 = tokensAfterExprOp) then
        failwith("expecting identifier or literal, but found " + tokensAfterExprValue2.Head)
      
      tokensAfterExprValue2
    
    else
      tokensAfterExprValue1

      
  // assignment tokens
  //
  // the BNF for assignemnt is : <assignment> -> identifier = <expr> ; 
  // so first we call matchTokens on identifer and then "=" sign and then
  // we make a call to expr function and finally check for the semicolon
  // and retunr the tokens
  
  and assignment tokens =
    let next_token : string = List.head tokens

    if next_token.StartsWith("identifier:") then
      
      let tokensAfterIdentifier = matchToken "identifier" tokens
      let tokensAfterEquals = matchToken "=" tokensAfterIdentifier
      let exprResult = expr tokensAfterEquals

      if (tokensAfterEquals = exprResult) then
        failwith("expecting identifier or literal, but found " + tokensAfterEquals.Head)
      
      matchToken ";" exprResult
    
    else
      tokens
      
    
  // according to bnf we should at least have one statment so we check for }
  // it means if there is no statment so we return the same tokens else we call stmt
  // until we reach the }
  
  let rec stmts tokens =
    
    let next_token = List.head tokens
    
    if (next_token = "}") then
      tokens
    
    else
      let T1 = stmt tokens
      stmts T1

  
  // simpleC tokens
  //
  // Given a list of token first it checks for void main() {} and then according
  // to BNF we need <stmts> which :
  // <stmts> -> <stmt> <morestmts>
  // <morestmts> -> <stmt> <morestmts>
  // | EMPTY
  // so we pass the tokens thourgh the stmts function to check each token for errors
  // and eventually check for EOF and return
  
  let private simpleC tokens = 
    
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6

    if (T7 = T6) then
      failwith ("expecting statement, but found " + T7.Head)

    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8 
    T9
    

  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid SimpleC program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  
  let parse tokens = 
    try
      let result = simpleC tokens
      "Success!"
    with 
      | ex -> "syntax_error: " + ex.Message
