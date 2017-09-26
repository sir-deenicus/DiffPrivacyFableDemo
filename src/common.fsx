module Commons

#load @"C:\prelude.fsx"

open Prelude.Common
open Prelude

module Map  =
       let sum m = Map.fold (fun sum _ x -> sum + x) 0. m

let stringr n x = string (round n x)

let stringr100 n x = string (round n (x * 100.))