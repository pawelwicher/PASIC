open System
open System.IO
open System.Text.RegularExpressions

let mutable callPointer = 0
let mutable vars : string list = []
let mutable ints : (string * int ref) list = []
let mutable decimals : (string * decimal ref) list = []
let mutable strings : (string * string ref) list = []

type CodeLine = {
    number : int
    commad : string
}

let (|ParseRegex|_|) pattern input =
   let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)
   if m.Success then Some (List.tail [for x in m.Groups -> x.Value]) else None

let parseCodeLine (line : string) =
    match line with
    | ParseRegex "(\d+)\s+(.+)" [a; b] -> { number = Int32.Parse a; commad = b }
    | _ -> failwith ("invalid syntax: " + line)

let isAlloacted (name : string) =
    let v = vars |> List.filter (fun x -> x = name) |> List.tryHead
    match v with
    | None -> ()
    | _ -> failwith (sprintf "variable '%s' is already used." name)

let invalidVariable (name : string) =
    failwith (sprintf "invalid variable: '%s'" name)

let allocateInt (name : string) (value : int) =
    isAlloacted name
    ints <- (name, ref value) :: ints
    vars <- name :: vars

let setValueInt (name : string) (value : int) =
    let i = ints |> List.tryFind (fun (n, _) -> n = name)
    match i with
    | Some(_, x) -> x := value
    | _ -> invalidVariable name

let allocateDecimal (name : string) (value : decimal) =
    isAlloacted name
    decimals <- (name, ref value) :: decimals
    vars <- name :: vars

let setValueDecimal (name : string) (value : decimal) =
    let i = decimals |> List.tryFind (fun (n, _) -> n = name)
    match i with
    | Some(_, x) -> x := value
    | _ -> invalidVariable name

let allocateString (name : string) (value : string) =
    isAlloacted name
    strings <- (name, ref value) :: strings
    vars <- name :: vars

let setValueString (name : string) (value : string) =
    let i = strings |> List.tryFind (fun (n, _) -> n = name)
    match i with
    | Some(_, x) -> x := value
    | _ -> invalidVariable name

let func (a : string) (b : string) 
    (f_i : int ref -> int ref -> unit)
    (f_d : decimal ref -> decimal ref -> unit)
    (f_s  : string ref -> string ref -> unit) =
    let i1 = ints |> List.tryFind (fun (n, _) -> n = a)
    let i2 = ints |> List.tryFind (fun (n, _) -> n = b)
    let d1 = decimals |> List.tryFind (fun (n, _) -> n = a)
    let d2 = decimals |> List.tryFind (fun (n, _) -> n = b)
    let s1 = strings |> List.tryFind (fun (n, _) -> n = a)
    let s2 = strings |> List.tryFind (fun (n, _) -> n = b)
    match i1, i2, d1, d2, s1, s2 with
    | Some(_, x), Some(_, y), None, None, None, None -> f_i x y
    | None, None, Some(_, x), Some(_, y), None, None -> f_d x y
    | None, None, None, None, Some(_, x), Some(_, y) -> f_s x y
    | _ -> invalidVariable a

let add a b =
    func 
        a
        b
        (fun (x : int ref) (y : int ref) -> x := !x + !y)
        (fun (x : decimal ref) (y : decimal ref) -> x := !x + !y)
        (fun (x : string ref) (y : string ref) -> x := !x + !y)

let sub a b =
    func 
        a
        b
        (fun (x : int ref) (y : int ref) -> x := !x - !y)
        (fun (x : decimal ref) (y : decimal ref) -> x := !x - !y)
        (fun (x : string ref) (y : string ref) -> failwith "'sub' on strings is not allowed")

let mul a b =
    func 
        a
        b
        (fun (x : int ref) (y : int ref) -> x := !x * !y)
        (fun (x : decimal ref) (y : decimal ref) -> x := !x * !y)
        (fun (x : string ref) (y : string ref) -> failwith "'mul' on strings is not allowed")

let goto (name : string) (line : int) =
    let i = ints |> List.tryFind (fun (n, _) -> n = name)
    match i with
    | Some(_, x) when !x = 0 -> ()
    | Some(_, _) -> callPointer <- line - 1
    | _ -> invalidVariable name

let print (name : string) =
    let i = ints |> List.tryFind (fun (n, _) -> n = name)
    let d = decimals |> List.tryFind (fun (n, _) -> n = name)
    let s = strings |> List.tryFind (fun (n, _) -> n = name)
    match i, d, s with
    | Some(_, v), None, None -> printfn "%i" !v
    | None, Some(_, v), None -> printfn "%A" !v
    | None, None, Some(_, v) -> printfn "%s" !v
    | _ -> invalidVariable name

let execute (command : string) =
    match command with
    | ParseRegex "^let\s+([a-z][a-z0-9]*)\s*=\s*(-?\d+)$" [name; value] -> allocateInt name (Int32.Parse value)
    | ParseRegex "^([a-z][a-z0-9]*)\s*=\s*(-?\d+)$" [name; value] -> setValueInt name (Int32.Parse value)
    | ParseRegex "^let\s+([a-z][a-z0-9]*)\s*=\s*(-?\d+\.\d+)$" [name; value] -> allocateDecimal name (Decimal.Parse value)
    | ParseRegex "^([a-z][a-z0-9]*)\s*=\s*(-?\d+\.\d+)$" [name; value] -> setValueDecimal name (Decimal.Parse value)
    | ParseRegex "^let\s+([a-z][a-z0-9]*)\s*=\s*\"(.*)\"$" [name; value] -> allocateString name value
    | ParseRegex "^([a-z][a-z0-9]*)\s*=\s*\"(.*)\"$" [name; value] -> setValueString name value
    | ParseRegex "^add\s+([a-z][a-z0-9]*)\s+([a-z][a-z0-9]*)$" [a; b] -> add a b
    | ParseRegex "^sub\s+([a-z][a-z0-9]*)\s+([a-z][a-z0-9]*)$" [a; b] -> sub a b
    | ParseRegex "^mul\s+([a-z][a-z0-9]*)\s+([a-z][a-z0-9]*)$" [a; b] -> mul a b
    | ParseRegex "^goto\s+([a-z][a-z0-9]*)\s+(\d+)$" [a; line] -> goto a (Int32.Parse line)
    | ParseRegex "^print\s+([a-z][a-z0-9]*)$" [name] -> print name
    | _ -> failwith ("invalid command: " + command)

let run (filePath : string) =
    let codeLines = File.ReadAllLines(filePath) |> Array.map parseCodeLine
    while callPointer < codeLines.Length do
        execute codeLines.[callPointer].commad
        callPointer <- callPointer + 1

[<EntryPoint>]
let main args =    
    try
        run args.[0]
    with
    | ex -> printfn ":: ERROR :: %s" ex.Message
    0