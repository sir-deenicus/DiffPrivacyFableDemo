module DiffPrivacyFJS

open Discrete.ProbabilityMonad
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import 
open Wrappers

let keepLeft f (x,y) = x , f y

let round (n:int) (x:float) = System.Math.Round(x,n)

let stringr n x = string (round n x)

let asEvent f = U2.Case1(System.Func<_,_>(f))
/////////////////////////////

//writing things this way makes it clearer, making mistakes is harder  
//but changes are easier
let computeRandomizedResponse p =
    distr { let! a = dualCase ("Against🛑 bacon🥓", p) "For bacon🥓" 
            let! b = bernoulli 0.5
            if b then return a
            else return "For bacon🥓" }
          
            
let diffSandwich p = 
    distr { let! a = categorical ["hotdog🌭",0.1;"Sandwich🥖",0.3;"Vegan Hamburger🍔",0.6] 
            let! b = bernoulli p
            if b then return a
            else let! f = uniform ["hotdog🌭";"Sandwich🥖";"Vegan Hamburger🍔"] 
                 return f}                           


let sandwichUpdate q p =
    let es =  Browser.document.getElementsByName "points2"  
    for i in 0..int es.length - 1 do es.[i].textContent <- string (round 2 p)
         
    let a1 = Browser.document.getElementById "rra1"
    let a2 = Browser.document.getElementById "rra2"
    let a3 = Browser.document.getElementById "rra3"
    let rrq = Browser.document.getElementById "rrq"

    a1.textContent <- stringr 2 (0.5 + 0.5 * (1. - p))
    a2.textContent <- stringr 2 (0.5 * p)    
    a3.textContent <- stringr 2 (2. * q)
    rrq.textContent <- stringr 2 q


let doUpdate q p =
    let es =  Browser.document.getElementsByName "rrp"  
    for i in 0..int es.length - 1 do es.[i].textContent <- string (round 2 p)
         
    let a1 = Browser.document.getElementById "rra1"
    let a2 = Browser.document.getElementById "rra2"
    let a3 = Browser.document.getElementById "rra3"
    let rrq = Browser.document.getElementById "rrq"

    a1.textContent <- stringr 2 (0.5 + 0.5 * (1. - p))
    a2.textContent <- stringr 2 (0.5 * p)    
    a3.textContent <- stringr 2 (2. * q)
    rrq.textContent <- stringr 2 q


let runPlot f divid p =
    let d = f p
    let survey = histogram d |> Array.map (keepLeft ((*) 100.))

    let title = Array.map (fun (l,p) -> l + ": " + (string (round 0 p)) + "%") survey |> String.concat " | "
   
    Plotly.barplot ("Survey Results<br>" + title) survey divid 
    survey



let foodSlilderChanged (src:Browser.HTMLInputElement) =
    let el = Browser.document.getElementsByName "fbias"

    el.[0].textContent <- src.value + "%"
    //el.[1].textContent <- src.value + "%"

    let rrslider = Browser.document.getElementsByName "points2" 

    rrslider.[0]?value <- src.value
    //rrslider.[1]?value <- src.value

    ignore <| runPlot diffSandwich "fsurvey" (float src.value * 0.01)


let sliderChanged (src:Browser.HTMLInputElement) =
    let el = Browser.document.getElementsByName "rrpercno"

    el.[0].textContent <- src.value + "%"
    el.[1].textContent <- src.value + "%"

    let rrslider = Browser.document.getElementsByName "points" 

    rrslider.[0]?value <- src.value
    rrslider.[1]?value <- src.value

    let p = float src.value * 0.01
    let survey = runPlot computeRandomizedResponse "rrcorr" p

    doUpdate (snd survey.[0] * 0.01) p


let init() =  
    Browser.window?foodSlilderChanged <- foodSlilderChanged
    Browser.window?sliderChanged <- sliderChanged

    ignore <| runPlot computeRandomizedResponse "rrcorr"  0.2
    ignore <| runPlot diffSandwich "fsurvey"  0.4

    doUpdate 0.1 0.2

    ()

init()