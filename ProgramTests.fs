module NUnitTestProject1

open NUnit.Framework
open FsUnit
open FsCheck

open Program

[<Test>]
let ``Check can get valid add operator`` () =
    [|"add"; "1"; "2"|] |> checkArgAmountGetOperator |>
    function
    |Ok k -> k |>  should equal (Add, seq ["1"; "2"])

[<Test>]
let ``Check can get error for invalid operator`` () =
    [|"notadd"; "1"; "2"|] |> checkArgAmountGetOperator |>
    function
    |Error e -> e |> should equal InvalidOperation


[<Test>]
let ``Check can get input back if valid arg amount`` () =
     [|"1"; "2"; "3"; "4"; "5"|] |> checkArgAmount |> 
    function
    |Ok k -> k |> should equal [|"1"; "2"; "3"; "4"; "5"|]

[<Test>]
let ``Check can get Error back if invalid arg amount`` () =
    [|"1"|] |> checkArgAmount |> 
    function
    |Error e -> e |> should equal InvalidArgAmount


[<Test>]
let ``Check can get arguments parsed to Number type`` () =
    [|"add"; "1"; "2"|]
    |>checkArgAmountGetOperator
    |>tryParseArgs
    |>function
        |Ok k ->
            match k with
            |(x,y) -> 
                match y with 
                |Ok o -> o |> should equal (seq [IntNum (Integer 1); IntNum (Integer 2)])

[<Test>]
let ``Check can get Error if not parsed to Number type`` () =
    [|"add"; "a1"; "b2"|]
    |>checkArgAmountGetOperator
    |>tryParseArgs
    |>function
        |Ok k ->
            match k with
            |(x,y) -> 
                match y with 
                |Error e -> e |> should equal ArgumentsNotNumeric

[<Test>]
let ``Check can parse to Int and get Number type`` () = 
    ["1";"2"]
    |> tryParseArgsToInts
    |> function
        |Ok k -> k |> should equal (seq [IntNum (Integer 1); IntNum (Integer 2)])

[<Test>]
let ``Check can parse to decimal and get Number type`` () = 
    ["1.1";"2.2"]
    |> tryParseArgsToInts
    |> function
        |Ok k -> k |> should equal (seq [FloatNum (Decimal 1.1); FloatNum (Decimal 2.2)])

[<Test>]
let ``Check can get Integer result if valid operation&args`` () =
    [|"add"; "1"; "2"|]
    |> checkArgAmountGetOperator
    |> tryParseArgs
    |> Result.bind doOperation
    |> function
        |Ok k -> k |> should equal (IntNum (Integer 3))

[<Test>]
let ``Check can get Decimal result if valid operation&args`` () =
    [|"add"; "1.1"; "2.2"|]
    |> checkArgAmountGetOperator
    |> tryParseArgs
    |> Result.bind doOperation
    |> function
        |Ok k -> k |> should equal (FloatNum (Decimal 3.3))

