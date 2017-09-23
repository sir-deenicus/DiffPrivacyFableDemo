module DiffPrivacyFJS

open Discrete.ProbabilityMonad
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import 
open Wrappers

let flip f x y = f y x

let konst x _ = x

let keepLeft f (x,y) = x , f y

let round (n:int) (x:float) = System.Math.Round(x,n)

let stringr n x = string (round n x)

let asEvent f = U2.Case1(System.Func<_,_>(f))

let extractOrigPop adjprob honestProb probFinal = (probFinal - ((1. - honestProb) * adjprob))/honestProb

let foreachNodeList f (n:Browser.NodeListOf<Browser.Element>) =
    for i in 0..int n.length - 1 do f n.[i]
/////////////////////////////

//writing things this way makes it clearer, making mistakes is harder  
//but changes are easier
let computeRandomizedResponse p =
    distr { let! a = dualCase ("Hate bacon 🛑🥓", p) "Love bacon ❤🥓" 
            let! b = bernoulli 0.5
            if b then return a
            else return "Hate bacon 🛑🥓" }
          

let diffSandwich p = 
    distr { let! a = categorical ["hotdog🌭",0.1;"Sandwich🥖",0.3;"Vegan Hamburger🍔",0.6] 
            let! b = bernoulli p
            if b then return a
            else let! f = uniform ["hotdog🌭";"Sandwich🥖";"Vegan Hamburger🍔"] 
                 return f}                           


let sandwichUpdate q p =
    let es =  Browser.document.getElementsByName "points2"  
    for i in 0..int es.length - 1 do es.[i].textContent <- string (round 2 p)
         
    let a1  = Browser.document.getElementById "rra1"
    let a2  = Browser.document.getElementById "rra2"
    let a3  = Browser.document.getElementById "rra3"
    let rrq = Browser.document.getElementById "rrq"

    a1.textContent  <- stringr 2 (0.5 + 0.5 * (1. - p))
    a2.textContent  <- stringr 2 (0.5 * p)    
    a3.textContent  <- stringr 2 (2.0 * q)
    rrq.textContent <- stringr 2 q

    
    Plotly.render_katex "c = \\pm\\sqrt{a^2 + b^2}" (Browser.document.getElementById "math3")


let doUpdate q p =
    let es =  Browser.document.getElementsByName "rrp"  
    
    for i in 0..int es.length - 1 do es.[i].textContent <- string (round 2 p)
         
    let a1  = Browser.document.getElementById "rra1"
    let a2  = Browser.document.getElementById "rra2"
    let a3  = Browser.document.getElementById "rra3"
    let rrq = Browser.document.getElementById "rrq"

    a1.textContent  <- stringr 2 (0.5 * (1. - p))
    a2.textContent  <- stringr 2 (0.5 + 0.5 * p)    
    a3.textContent  <- stringr 2 (2. * (q-0.5))
    rrq.textContent <- stringr 2 q


let runPlot f divid p =
    let d = f p
    let survey = histogram d |> Array.map (keepLeft ((*) 100.))

    let title = Array.map (fun (l,p) -> l + ": " + (string (round 0 p)) + "%") survey |> String.concat " | "
   
    Plotly.barplot ("Survey Results<br>" + title) survey divid 
    survey


let updateEquations b (hist:Map<_,_>) =
    let coinbs = Browser.document.getElementsByName "coinb" 
    let coinS = Browser.document.getElementsByName "coinS" 

    foreachNodeList (fun e -> e.textContent <- stringr 2 b) coinbs

    foreachNodeList (fun e -> 
           let q = hist.[e.id] * 0.01
           let p = extractOrigPop (1./3.) b q
           e.textContent <- sprintf "%s = (%s" (stringr 1 p) (stringr 2 q)) coinS


let foodSlilderChanged (src:Browser.HTMLInputElement) =
    let el = Browser.document.getElementsByName "fbias"

    foreachNodeList (fun e -> e.textContent <- src.value + "%") el

    let rrslider = Browser.document.getElementsByName "points2" 

    foreachNodeList (fun e -> e?value <- src.value) rrslider 

    let b = (float src.value * 0.01)
    let hist = runPlot diffSandwich "fsurvey" b |> Map

    updateEquations b hist


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
    runPlot diffSandwich "fsurvey"  0.4 |> Map |> updateEquations 0.4

    doUpdate 0.6 0.2

    ()

init()