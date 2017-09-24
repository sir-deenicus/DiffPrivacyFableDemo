module Commons 

open Fable.Core.JsInterop
open Fable.Import 
open Fable.Core
open System

let flip f x y = f y x

let konst x _ = x

let keepLeft f (x,y) = x , f y

let keepRight f (x,y) = f x , y

let round (n:int) (x:float) = System.Math.Round(x,n)

let stringr n x = string (round n x)

let stringr100 n x = string (round n (x * 100.))
///////////////////

module Map  =
       let sum m = Map.fold (fun sum _ x -> sum + x) 0. m

module Array =
   let inline normalizeWeights (a:_[]) =
       let sum = Array.sumBy snd a 
       Array.map (fun (x,w) -> x, w/sum) a

   let indexed a = Array.mapi (fun i x -> (i,x)) a
   
   let selectColumn c a = 
       Array.map (indexed >> Array.filter (fst >> (=) c) >> Array.map snd) a  

module String = 
   let pad padlen (s:string) = s + String.replicate (max 0 (padlen - s.Length)) " "
   let inline toupper (str:string) = str.ToUpper()
   

let inline joinToStringWith sep (s:'a seq) = String.Join(sep, s)           
////////////
let buildTableRow (collens:_[]) (row:string[]) =
       row |> Array.mapi (fun i s ->  String.pad collens.[i] s) |> joinToStringWith " | "


let makeTable newline headers title (table:string[][]) =
       let hlen = Array.map String.length headers

       let lens = table |> Array.map (Array.map (String.length))

       let longest = [|for c in 0..headers.Length - 1 -> max hlen.[c] (Array.selectColumn c lens |> Array.map Seq.head |> Array.max)|]

       let t0 = table |> Array.map (buildTableRow longest) |> joinToStringWith newline

       let hrow = [|headers; [|for i in 0..headers.Length - 1 -> String.replicate longest.[i] "-"|]|] 
                  |> Array.map (buildTableRow longest) 
                  |> joinToStringWith newline
       String.Format("{0}{1}{1}{2}{1}{3}", (String.toupper title), newline, hrow, t0)

////////////////////////

let foreachNodeList f (n:Browser.NodeListOf<Browser.Element>) =
    for i in 0..int n.length - 1 do f n.[i]

let asEvent f = U2.Case1(System.Func<_,_>(f))

