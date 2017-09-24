module DiffPrivacyFJS

open Commons
open Discrete.ProbabilityMonad
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import 
open Wrappers

let extractOrigPop adjprob honestProb probFinal = (probFinal - ((1. - honestProb) * adjprob))/honestProb

/////////////////////////////
let foodlabels = dict ["hotdog🌭","hotdog";"Sandwich🥖","sandwich";"Vegan Hamburger🍔","burger"] 

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
    let survey = pmf d |> Array.map (keepLeft ((*) 100.))

    let title = Array.map (fun (l,p) -> l + ": " + (string (round 0 p)) + "%") survey |> String.concat " | "
   
    Plotly.barplot ("Survey Results<br>" + title) survey divid 
    survey


let updateEquations b (hist:Map<_,_>) =
    let coinS = Browser.document.getElementsByName "coinS" 
    
    foreachNodeList (fun e -> 
           let q = hist.[e.id] * 0.01
           let p = extractOrigPop (1./3.) b q
           let equation = sprintf "p_{%s}=%s=\\frac{(p_{survey}=%s)-(1/3)(1 - %s)}{p_{heads}=%s}"
                                   foodlabels.[e.id] (stringr 1 p) (stringr 2 q) (stringr 2 b) (stringr 2 b)   
           Plotly.render_katex equation e) coinS


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

////////////////////////

let food = ["fish";"meat";"cheese"]
let toys = ["bone";"ball of yarn";"egg carton"; "cardboard box"]

let conditionalDistr (ps:_[]) items animal = 
    distr { 
        let dist = 
                match animal with
                | "cat" -> ps.[0]
                | "dog" -> ps.[1]
                | _ ->     ps.[2]
        return! categorical (List.zip items dist)
    }
    
let favfood animal = 
    conditionalDistr [|[0.5;0.4;0.1] ; [0.3;0.6;0.1] ; [0.01; 0.001;0.989]|] food animal
    
let favtoy animal = 
    conditionalDistr [| [0.01;0.4;0.05;0.54] ; 
                        [0.75;0.1;0.05;0.1]  ; 
                        [0.01;0.05;0.65 ;0.29]|] toys animal
    
let hadcatnipInLastWeek animal = 
    conditionalDistr [|[0.7;0.3] ;[0.2;0.8] ;[0.35; 0.65]|] ["yes";"no"] animal


let favfood2 animal = uniform food
    
let favtoy2 animal = uniform toys
    
let hadcatnipInLastWeek2 animal = uniform ["yes";"no"] 

let multipleRequests p a = distr {
         let! b  = bernoulli p
         let! bf = bernoulli p
         let! bt = bernoulli p
         let! bc = bernoulli p
         let! ar = uniform ["cat";"dog";"mouse"] 

         let animal = if b then a else ar
         let! f = if bf then favfood a else uniform food
         let! t = if bt then favtoy a else uniform toys
         let! c = if bc then uniform ["yes";"no"] else hadcatnipInLastWeek a
         return (animal,f,t,c)
        }


let jointprobAnimal a = distr {                       
    let! f = favfood a 
    let! t = favtoy a 
    let! c = hadcatnipInLastWeek a
    return (f,t,c) }

let jointProbIndependent _ = distr {                       
     let! f = uniform food 
     let! t = uniform toys 
     let! c = uniform ["yes";"no"]
     return (f,t,c)
    }

let queryAnimal p a = distr { let! b = bernoulli p
                              if b then return a 
                              else return! uniform ["cat";"dog";"mouse"] }

let unicodeDecorate = function "cat" -> "cat🐈" | "dog" -> "dog🐶" | _ -> "mouse🐭"
//------------

let guess =  uniform ["cat";"dog";"mouse"] 

let mutable prior = Map (pmf guess)

let guessMult =   uniform ["cat",1./3.;"dog",1./3.;"mouse",1./3.] 

let mutable priorMult = pmf guessMult |> Map

let mutable totguesses = 0

let updateText() = 
    let str1 = 
        prior
        |> pmfWeighted 
        |> Map.toArray 
        |> Array.map (fun (v, p) -> unicodeDecorate v + ": " + (stringr100 0 p) + "%")
        |> String.concat " | "    
    
    let str2 = 
        priorMult
        |> pmfWeighted 
        |> Map.toArray 
        |> Array.map (fun ((v,_), p) -> unicodeDecorate v + ": " + (stringr100 0 p) + "%")
        |> String.concat " | "

    let info = Browser.document.getElementById("guesserInf")
    let countdisp = Browser.document.getElementById("nqueries") :?> Browser.HTMLSpanElement

    info.innerHTML <- sprintf "<b>Correlator's Beliefs</b>: %s<br/><br/><b>Multiple Same Question Asker's Beliefs</b>: %s" str1  str2
    
    countdisp.textContent <- string totguesses

let resetGuesses () = 
    priorMult <- Map (pmf guessMult)
    prior <- Map (pmf guess)
    totguesses <- 0
    updateText()

let updateGuesses () =

    let corrtype = Browser.document.querySelector "input[name=corropt]:checked" :?> Browser.HTMLInputElement
    let speciesopts = Browser.document.getElementById("species") :?> Browser.HTMLSelectElement
    
    let species = speciesopts.options.[int speciesopts.selectedIndex] :?> Browser.HTMLOptionElement

    let jointProb = if corrtype.value = "correlated" then jointprobAnimal else jointProbIndependent 

    let reqs   = (multipleRequests 0.52 species.value).Sample
    let animal = (queryAnimal 0.52 species.value).Sample

    prior <- update (fun (_,food,toy,catnip) species -> probabilityOf ((=) (food,toy,catnip)) (jointProb species)) prior reqs 
    priorMult <- update (fun species (speciesHypth,p) -> if speciesHypth = species then (1. - p) else p) priorMult animal  
    totguesses <- totguesses + 1
    updateText()
//////////////////////////

let init() =  
    Browser.window?foodSlilderChanged <- foodSlilderChanged
    Browser.window?sliderChanged <- sliderChanged
    Browser.window?updateGuesses <- updateGuesses
    Browser.window?resetGuesses <- resetGuesses

    ignore <| runPlot computeRandomizedResponse "rrcorr"  0.2
    runPlot diffSandwich "fsurvey"  0.4 |> Map |> updateEquations 0.4

    doUpdate 0.6 0.2

    updateText()

init()