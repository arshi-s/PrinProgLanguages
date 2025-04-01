
let validVarNames = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"];;

let partition (input: string list) (bound : string) : string list list = (* takes 2 params, input & delimiter/bound*)
   let rec helper result currPartition remlst = 
    match remlst with 
    | [] -> result @ [currPartition] (* add currentPartition to result *)
    | (h::t) ->
        if h = bound then (* if h = delimiter/bound means ends of currPartition*)
          helper (result @ [currPartition]) [] t (*add currPartition to result & start new partition w/[] & continue on rem elements t*)
        else
          helper result (currPartition @ [h]) t (*if h != delimiter then add it to currPartition /same list of result and rem elements t*)
  in 
  helper [] [] input(* call helper within partition func*)
;;

let buildCNF (input : string list) : (string * string) list list = 
 let clauses = partition input "AND" in
  let rec parse_clause = function
    | "(" :: rest -> parse_clause rest
    | ")" :: _ -> []
    | literals -> List.rev (parse_literals literals [])
  and parse_literals literals acc =
    match literals with
    | [] -> acc
    | ")" :: _ -> acc (* Stop parsing when encountering ")" *)
    | "OR" :: t -> parse_literals t acc
    | "NOT" :: var :: t -> parse_literals t ((var, "NOT") :: acc)
    | var :: t -> parse_literals t ((var, "") :: acc)
  in
  List.map parse_clause clauses
;;

let getVariables (input : string list) : string list = 
  let rec getVarsHelper remTokens var_accumulator = (*rec func w/2 params remaining tokens to process and accumulator that stores vars done so far*)
    match remTokens with
    | [] -> var_accumulator (*if tokens list empty->processed all vars, return accumulated vars*)
    | h::t ->
        if List.mem h validVarNames && not (List.mem h var_accumulator) then (*checks if current token,h,valid var name and if it hasnt been encountered before*)
          getVarsHelper t (h :: var_accumulator) (*rec call on tails and changed acculuator*)
        else
          getVarsHelper t var_accumulator (*rec call on tails and add current var h to accumulator*)
  in (*end of inner function*)
  getVarsHelper input [] (*starts call to getVarsHelper with input and empty accumulator*)
;;

let rec generateDefaultAssignments (varList : string list) : (string * bool) list = 
  match varList with
  | [] -> []
  | (h::t) -> (h, false) :: generateDefaultAssignments t (*make the head/1st element false then recurse on tails*)
;;

let rec generateNextAssignments (assignList : (string * bool) list) : (string * bool) list * bool = 
   let rec addOne lst carry =
    match lst with
    | [] -> [("a", true)], carry (* if list empty, return first var set to true and cary*)
    | (var, false) :: t -> (var, true) :: t, false (*if current bit false, flip to true return false for carry *)
    | (var, true) :: t ->                      (* if current bit true *)
        if var = "a" then ((var, false) :: t, true) (* if its leftmost var, set it to false and return true for carry *)
        else
          let rest, new_carry = addOne t carry in (* rec add one to next bit *)
          if new_carry then ((var, false) :: rest, true) (* if theres an overflow/carry, flip current bit to false and return true for carry *)
          else ((var, false) :: rest, false) (* if no carry, flip current bit to false and return false for carry *)
  in
  let nextAssignments, carry = addOne (List.rev assignList) false in (*start with false carry and reverse list to start with rightmost bit first *)
  (List.rev nextAssignments), carry
;;

let rec lookupVar (assignList : (string * bool) list) (str : string) : bool = 
  match assignList with
  | [] -> failwith "Variable not found in assignment list"
  | (var, value) :: rest -> (*break input up into the first tuple and rest of input*)
      if var = str then
        value (*if the var=str ret the value*)
      else
        lookupVar rest str (*otherwise recurse on rest of input and do the same*)
;;

let evaluateCNF (t : (string * string) list list) (assignList : (string * bool) list) : bool = 
  let rec evaluateClause clause =
  match clause with
  | [] -> false (*if empty clause, return false*)
  | (var, neg) :: t ->
      match List.assoc_opt var assignList with
      | Some value ->
          let negation_status = if neg = "NOT" then "NOT" else "" in (*check if literals satisfied by assignment*)
          if (neg = "NOT" && not value) || (neg = "" && value) then true
          else evaluateClause t
      | None -> evaluateClause t (*var not found*)
  in
  let rec evaluateCNF' cnf =
    match cnf with
    | [] -> true 
    | h :: t' -> evaluateClause h && evaluateCNF' t'
  in
  evaluateCNF' t
;;

let satisfy (input : string list) : (string * bool) list =
  let cnf = buildCNF input in (*build CNF using func*)
  let variables = getVariables input in (*get var list*)

  let rec try_Varassignments assignList = (*recursive func to try var assignments*)
    if evaluateCNF cnf assignList then (*if the assignment works, ret some assignList*)
      Some assignList
    else
      let nextAssignments, carry = generateNextAssignments assignList in
      if carry then
        None (*if carry is true, return none*)
      else
        try_Varassignments nextAssignments (*if carry false, try next assignment*)
  in

  let rec find_assignment varList =
    match varList with
    | [] -> [("error", true)] (*if no vars left, return error*)
    | var :: tl ->
        let assignList = (var, true) :: List.map (fun v -> (v, false)) tl in
        match try_Varassignments assignList with
        | Some result -> result (*return satisfying assignment*)
        | None -> find_assignment tl (*if not, try next one*)
  in

  List.rev (find_assignment variables) (*reverse list*)
;;