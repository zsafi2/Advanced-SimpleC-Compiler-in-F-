//
// Analyzer for SimpleC programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
//
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//
//   [("x", "int"); ("y", "int"); ("z", "real")]
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

module analyzer =
  //
  // NOTE: all functions in the module must be indented.
  //

  let checkForDuplicates variables =
    
    let rec check seen = function
      | [] -> ()  // If the list is empty, no duplicates were found
      | (varName, _) :: rest ->
        if List.exists (fun x -> x = varName) seen then
            failwith ("redefinition of variable '" + varName + "'")
        else
            check (varName :: seen) rest  // Add the variable name to the list and continue

    check [] variables

  let rec rest_tokens tokens =
    List.tail tokens


  let rec findVariables tokens lst =
    match tokens with
    | [] -> lst
    | first :: second :: rest when first = "int" || first = "real" ->
        let varName = second.Split(':')[1]  // Extracts the variable name from "identifier:var_name"
        findVariables rest ((varName, first) :: lst)
    | _ :: rest -> findVariables rest lst  // Skip the current token
  
  let beforeDefinition tokens =
    
    let rec processTokens tokens definedVars =
        match tokens with
        | [] -> ()  // End of the token list
        | "int" :: second :: rest | "real" :: second :: rest ->
            match second.Split(':') with
            | [| "identifier"; varName |] -> 
                processTokens rest (varName :: definedVars)  // Add the varName to definedVars
            | _ -> failwith "Unexpected token format after type declaration"
        | first :: rest ->
            match first.Split(':') with
            | [| "identifier"; varName |] -> 
                if not (List.contains varName definedVars) then
                    failwith ("variable '" + varName + "' undefined")
                else
                    processTokens rest definedVars  // Move on, as this is a correct usage
            | _ -> processTokens rest definedVars  // Skip any tokens that do not match expected pattern
    
    processTokens tokens []

  let private simpleC tokens = 
    let symbtable = findVariables tokens []
    checkForDuplicates symbtable
    beforeDefinition tokens

    (tokens, symbtable)


  //
  // build_symboltable tokens
  //
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  //
  // On success, the symboltable is a list of tuples of the
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])

  
