// Learn more about F# at http://fsharp.org
open System

type Integer = bigint
type Decimal = decimal
type Number = 
    |IntNum of Integer
    |FloatNum of Decimal

    member inline this.GetValue<'a>(): 'a =
        match this with
        |IntNum x -> unbox<'a> x
        |FloatNum x -> unbox<'a> x

    member this.gType =
        match this with
        |IntNum x -> 1
        |FloatNum y -> 2

type Argument = string
type Arguments = Argument []


type InpError =
    |InvalidOperation
    |InvalidArgAmount
    |ArgumentsNotNumeric

let (>!) a b = if (a > b)
               then a
               else b

let (<!) a b = if (a < b)
               then a
               else b





type operation =
    |Add
    |Sub
    |Mult
    |Div
    |BigThan
    |SmallThan

let matchOperatorInt (op:operation)=
    match op with
    |Add -> (+)
    |Sub -> (-)
    |Mult -> (*)
    |Div -> (/)
    |BigThan -> (>!)
    |SmallThan -> (<!)

let matchOperatorFloat (op:operation)=
    match op with
    |Add -> (+)
    |Sub -> (-)
    |Mult -> (*)
    |Div -> (/)
    |BigThan -> (>!)
    |SmallThan -> (<!)



let getErrorMessage =
    function
    |InvalidOperation ->
        ("ERROR: Operation is not valid",2) 
    |InvalidArgAmount ->
        ("ERROR: Not enough arguments specified (expects one valid operation and at least two valid numbers)",3)
     |ArgumentsNotNumeric ->
        ("ERROR: one or more arguments are not valid numbers",4)

   

////////////////////////////////////////////////////////////////////////////////////////
let checkArgAmount (inp:Arguments)=
    if Array.length inp >= 3 
    then Ok inp
    else Error InvalidArgAmount

let getOp (argv:Arguments) = Ok (Seq.head argv,Seq.tail argv)

let checkOperator (k:Argument*seq<Argument>)=
    match k with
    |(a,x) -> a.ToLower() |> fun q ->
    match q with
    |"add" -> Ok(Add,x)
    |("sub") -> Ok(Sub,x)
    |("mult") -> Ok(Mult,x)
    |("div") -> Ok(Div,x)
    |("biggest") -> Ok(BigThan,x)
    |("smallest") -> Ok(SmallThan, x)
    |_ -> Error InvalidOperation

let checkArgAmountGetOperator argv= 
    argv 
    |> checkArgAmount
    |> Result.bind getOp
    |> Result.bind checkOperator




//////////////

let parseTheSequenceInt: seq<Argument> -> seq<bool*Integer> = Seq.map (Integer.TryParse)
let parseTheSequenceFloat: seq<Argument> -> seq<bool*Decimal> = Seq.map (Decimal.TryParse)


let checkAllSequenceIsValidInt : seq<bool * Integer> -> Result<seq<Number>, InpError> =
    fun args ->
    if Seq.forall fst args  
    then Seq.map (snd) args |> Seq.map (Number.IntNum) |> Ok
    else Error ArgumentsNotNumeric

let checkAllSequenceIsValidFloat : seq<bool * Decimal> -> Result<seq<Number>, InpError> =
    fun args ->
    if Seq.forall fst args  
    then Seq.map (snd) args |> Seq.map (Number.FloatNum) |> Ok
    else Error ArgumentsNotNumeric

let tryParseArgsToInts (vl:seq<Argument>) = 
    vl |> parseTheSequenceInt |> checkAllSequenceIsValidInt
    |> fun k -> 
        match k with
        | Ok ok -> k
        | Error er -> vl |> parseTheSequenceFloat |> checkAllSequenceIsValidFloat
                      |> fun m -> m

////////////
let tryParseArgs (vl:Result<(operation*seq<Argument>),InpError>)= 
     match vl with
     |Ok (op,argSeq) -> argSeq |> tryParseArgsToInts |> fun x -> Ok (op,x)
     |Error r -> Error r


let getResultInt (opSeq:operation*seq<Integer>)=
    match opSeq with
    |(op,intseq) -> 
        intseq
          |> Seq.reduce (fun s1 s2 -> matchOperatorInt op s1 s2)
          |> IntNum

let getResultDec (opSeq:operation*seq<Decimal>)=
    match opSeq with
    |(op,decseq) -> 
        decseq
          |> Seq.reduce (fun s1 s2 -> matchOperatorFloat op s1 s2)
          |> FloatNum


let doOperation (a:operation*Result<seq<Number>,InpError>)=
    match a with
    |(op,y) ->
        match y with
        |Ok numseq -> 
                      if (numseq|> Seq.head |> fun d -> d.gType = 1) 
                      then numseq |> Seq.map (fun vl -> vl.GetValue<Integer>()) |> fun intseq ->  getResultInt (op,intseq) |> Ok
                      else numseq |> Seq.map (fun vl -> vl.GetValue<Decimal>()) |> fun decseq -> getResultDec (op,decseq) |> Ok

        |Error er -> Error er

let handleResult (vl:Result<Number,InpError>)=
           match vl with
           |Ok value ->
                     if (value.gType = 1)
                     then value.GetValue<Integer>() |> printfn "%A" 
                     else value.GetValue<Decimal>() |> printfn "%f" 
                     0
                     
           |Error errorVal ->
               errorVal |> getErrorMessage |> fun (s,i) -> s |> eprintfn "%s" |> fun d -> i


[<EntryPoint>]
let main (argv:Arguments) =
    argv
    |> checkArgAmountGetOperator
    |> tryParseArgs
    |> Result.bind doOperation
    |> handleResult
    


