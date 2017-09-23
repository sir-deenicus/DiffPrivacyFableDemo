module Wrappers.Plotly

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import 


[<Emit("bar_plot($1,$2,$3,$0)")>]
let barplotjs (divId:string) (title: string) (xs:'a[]) (ys:'b[]) = jsNative

[<Emit("col_plot($1,$2,$3,$0)")>]
let colplotjs (divId:string) (title: string) (xs:'a[]) (ys:'b[]) = jsNative


let barplot (title: string) (points:('a *'b)[]) (divId:string) = 
    let xs, ys = Array.unzip points
    barplotjs divId title xs ys 


let colplot (title: string) (data:('a *'b)[]) (divId:string) = 
    let xs, ys = Array.unzip data
    colplotjs divId title xs ys


[<Emit("katex.render($0,$1)")>]
let render_katex e d = jsNative