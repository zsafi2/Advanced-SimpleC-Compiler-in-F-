//
// Analyzer for SimpleC programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid SimpleC program.
//
// Modified by:
//   Zaheer Safi
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

module checker =
  //
  // NOTE: all functions in the module must be indented.
  //

  let rec checkOperationTypes tokens symbolTable =
    match tokens with
    | [] -> ()  // No more tokens to process
    | first :: operator :: third :: rest when List.contains operator ["+"; "-"; "*"; "/"; "^"] ->
        let type1 = getType first symbolTable operator
        let type2 = getType third symbolTable operator
        if type1 <> type2 || (type1 <> "int" && type1 <> "real") then
            failwith("type mismatch '" + type1 + "' " + operator + " '" + type2 + "'")
        checkOperationTypes rest symbolTable  // Continue checking the rest of the tokens
    | _ :: rest -> checkOperationTypes rest symbolTable  // Skip token and continue

  and getType token symbolTable operator =
    match token with
    | s when s.StartsWith("int_literal:") -> "int"
    | s when s.StartsWith("real_literal:") -> "real"
    | s when s.StartsWith("identifier:") ->
        let varName = s.Substring("identifier:".Length)
        let (_, typ) = symbolTable |> List.find (fun (name, _) -> name = varName)
        typ
    | _ -> failwith ("operator " + operator + " must involve 'int' or 'real'")
  
  let rec checkComparisonTypes tokens symbolTable =
    match tokens with
    | [] -> ()  // No more tokens to process
    | first :: operator :: third :: rest when List.contains operator ["<"; "<="; ">"; ">="; "=="; "!="] ->
        let type1 = getTypeComp first symbolTable
        let type2 = getTypeComp third symbolTable
        // Check if both sides are booleans or if they are the same non-boolean type
        if (type1 = "bool" && type2 = "bool") || (type1 = type2 && type1 <> "bool") then
            checkComparisonTypes rest symbolTable  // Continue checking the rest of the tokens
        else
            failwith ("type mismatch '" + type1 + "' " + operator + " '" + type2 + "'")
    | _ :: rest -> checkComparisonTypes rest symbolTable  // Skip token and continue
  
  and getTypeComp token symbolTable =
    match token with
    | "true" | "false" -> "bool"
    | s when s.StartsWith("int_literal:") -> "int"
    | s when s.StartsWith("real_literal:") -> "real"
    | s when s.StartsWith("str_literal:") -> "str"
    | s when s.StartsWith("identifier:") ->
        let varName = s.Substring("identifier:".Length)
        let (_, typ) = symbolTable |> List.find (fun (name, _) -> name = varName)
        typ
    | _ -> failwith ("operator " + token + " must involve 'int' or 'real'")
  
  let rec checkAssignmentTypes tokens symbolTable =
    match tokens with
    | [] -> ()  // No more tokens to process
    | first :: "=" :: expr :: rest when first.StartsWith("identifier:") ->
        let varName = first.Substring("identifier:".Length)
        let id_type = getTypeAssign first symbolTable
        let expr_type = getTypeAssign expr symbolTable
        if id_type <> expr_type && not (id_type = "real" && expr_type = "int") then
            failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'")
        checkAssignmentTypes rest symbolTable  // Continue checking the rest of the tokens
    | _ :: rest -> checkAssignmentTypes rest symbolTable  // Skip token and continue

  and getTypeAssign token symbolTable =
    
    match token with
    | "true" | "false" -> "bool"
    | s when s.StartsWith("int_literal:") -> "int"
    | s when s.StartsWith("real_literal:") -> "real"
    | s when s.StartsWith("str_literal:") -> "str"
    | s when s.StartsWith("identifier:") ->
        let varName = s.Substring("identifier:".Length)
        match symbolTable |> List.tryFind (fun (name, _) -> name = varName) with
        | Some (_, typ) -> typ
        | None -> failwith ("Unexpected error: identifier not found in symbol table, but should exist.")
    | _ -> failwith ("Unexpected token: " + token)
  
  let warnRealEquality tokens symbolTable =
    let rec checkTokens tokens =
        match tokens with
        | [] -> ()  // End of token list
        | first :: "==" :: second :: rest ->
            let type1 = getType first symbolTable
            let type2 = getType second symbolTable
            if type1 = "real" || type2 = "real" then
                printfn "warning: comparing real numbers with == may never be true"
            checkTokens rest  // Continue checking the rest of the tokens
        | _ :: rest -> checkTokens rest  // Skip the current token and continue

    and getType token symbolTable =
        match token with
        | "true" | "false" -> "bool"
        | s when s.StartsWith("int_literal:") -> "int"
        | s when s.StartsWith("real_literal:") -> "real"
        | s when s.StartsWith("identifier:") ->
            let varName = s.Substring("identifier:".Length)
            match symbolTable |> List.tryFind (fun (name, _) -> name = varName) with
            | Some (_, typ) -> typ
            | None -> "Error: identifier not found in symbol table"  // Safety check for symbol table integrity
        | _ -> "Unrecognized type"  // Handle unexpected token types

    checkTokens tokens

  let checkIfConditionTypes tokens symbolTable =
    
    let rec findCondition tokens =
        match tokens with
        | [] -> ()  // End of tokens
        | "if" :: "(" :: rest -> 
            let condition, remainder = extractCondition rest []
            let expr_type = evaluateConditionType condition symbolTable
            if expr_type <> "bool" then
                failwith ("if condition must be 'bool', but found '" + expr_type + "'")
            findCondition remainder  // Continue checking the rest of the tokens
        | _ :: rest -> findCondition rest  // Skip tokens and continue

    and extractCondition tokens acc =
        match tokens with
        | ")" :: rest -> (List.rev acc, rest)  // Found the end of condition
        | token :: rest -> extractCondition rest (token :: acc)  // Accumulate condition tokens
        | [] -> failwith "Mismatched parentheses in if condition"  // Error handling for unmatched "("

    and evaluateConditionType tokens symbolTable =
        let hasComparison = List.exists (fun token -> List.contains token ["<"; "<="; ">"; ">="; "=="; "!="]) tokens
        if hasComparison then
            "bool"  // Assume comparison returns bool
        else
            evaluateExpressionType tokens symbolTable

    and evaluateExpressionType tokens symbolTable =
        match tokens with
        | [single] -> literalType single  // Evaluate single token directly
        | _ -> "int_literal"  // Default for complex expressions assuming integer results (simplification)

    and literalType token =
        match token with
        | "true" | "false" -> "bool"
        | s when s.StartsWith("int_literal:") -> "int"
        | s when s.StartsWith("real_literal:") -> "real"
        | s when s.StartsWith("str_literal:") -> "str"
        | s when s.StartsWith("identifier:") ->
            let varName = s.Substring("identifier:".Length)
            match symbolTable |> List.tryFind (fun (name, _) -> name = varName) with
            | Some (_, typ) -> typ
            | None -> failwith ("Error: identifier not found in symbol table")
        | _ -> "unknown"

    findCondition tokens
  
  
  let private simpleC tokens symboltable = 
    
    warnRealEquality tokens symboltable
    checkOperationTypes tokens symboltable
    checkComparisonTypes tokens symboltable
    checkAssignmentTypes tokens symboltable
    checkIfConditionTypes tokens symboltable
    
    tokens


  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message

