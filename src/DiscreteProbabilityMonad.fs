module Discrete.ProbabilityMonad

open Commons

let rnd = System.Random()                

//from Expert F# code by Don Syme
//added some helpers to make the code more easily express bayesian formalisms

type Distribution<'T when 'T : comparison> =
    abstract Sample : 'T
    abstract Support : Set<'T>
    abstract Expectation: ('T -> float) -> float
                                                          
let always x =
    { new Distribution<'T> with
    member d.Sample = x
    member d.Support = Set.singleton x
    member d.Expectation(H) = H(x) }       


let coinFlip (p:float) (d1:Distribution<'T>) (d2:Distribution<'T>) =
    if p < 0.0 || p > 1.0 then failwith "invalid probability in coinFlip"
    { new Distribution<'T> with
        member d.Sample =         
            if rnd.NextDouble() < p then d1.Sample else d2.Sample
        member d.Support = Set.union d1.Support d2.Support
        member d.Expectation(H) =     
          p * d1.Expectation(H) + (1.0-p) * d2.Expectation(H)}


let bind (dist:Distribution<'T>) (k: 'T -> Distribution<'U>) =
    { new Distribution<'U> with
        member d.Sample = (k dist.Sample).Sample
        member d.Support = Set.unionMany (dist.Support |> Set.map (fun d -> (k d).Support))
        member d.Expectation H = dist.Expectation(fun x -> (k x).Expectation H) }

/////////////////////////////////////

type DistributionBuilder() =
    member x.Delay f = bind (always ()) f
    member x.Bind (d, f) = bind d f
    member x.Return v = always v
    member x.ReturnFrom vs = vs
 
let distr = DistributionBuilder()  

///////////////////

let weightedCases (inp: ('T * float) list) =
    let rec coinFlips w l =
        match l with
            | [] -> failwith "no coinFlips"
            | [(d,_)] -> always d
            | (d,p)::rest -> coinFlip (p/(1.0-w)) (always d) (coinFlips (w+p) rest)
    coinFlips 0.0 inp

let countedCases inp =
    let total = Seq.sumBy (fun (_,v) -> v) inp
    weightedCases (inp |> List.map (fun (x,v) -> (x, float v / float total)))

/////////Helpers
let dualCase ((_,p) as o1) o2 = weightedCases [o1; o2 , 1. - p]

let bernoulli p = dualCase (true, p) false

let uniform (items:'a list) = 
    let num = float items.Length
    List.map (fun item -> item, 1./num) items |> weightedCases

let categorical distr = weightedCases distr    

////////////////////////////// 

let conditionalProb eventx condition  (t:Distribution<'a>) =
   let n = t.Expectation(fun c -> if eventx c && condition c then 1. else  0.)  

   let z = t.Expectation(fun c -> if condition c then 1. else  0.) 
   n / z


let probabilityOf eventx (t:Distribution<'a>) =
    t.Expectation(fun c -> if eventx c then 1. else  0.) 


let rec update likelihood state obsv = 
        Map.map (fun item (prob:float) -> prob * likelihood obsv item) state    

let updates likelihood state obsvs =
    List.fold (update likelihood) state obsvs


let probabilityOfWeighted eventx (weights:Map<_,_>)  =
    let n = Map.filter (fun k _ -> eventx k) weights |> Map.sum   
    let z = Map.sum weights  
    n / z

let probabilityOfWeighted2 eventx (weights:('a * float)[])  =
    let n = Array.filter (fun (k, _) -> eventx k) weights |> Array.sumBy snd
    let z = Array.sumBy snd weights  
    n / z

let pmfWeighted (weights:Map<_,_>)  =    
    let t = Map.sum weights  
    Map.map (fun _ p -> p/t) weights 

let pmf (dist:Distribution<_>) = 
    dist.Support 
    |> Set.toArray 
    |> Array.map (fun c -> c, probabilityOf ((=) c) dist)  


let histogram len (d:_[]) =
    let _, maxp = Array.maxBy snd d
    Array.map (fun (x,p:float)-> 
          [|sprintf "%A" x ;
            stringr100 2 p + "%"; 
            String.replicate (int(round 0 (p/maxp * len))) "#" |]) d
    |> makeTable "\n" [|"item";"p"; ""|] ""  

/////////////
    
let toBits x = x / log 2. 

let inline log0 x = if x = 0. then 0. else log x

let inline entropy dist = -Seq.sumBy (fun (_,p) -> p * log0 p) dist

let entropyDistr d = entropy (pmf d)

let inline conditionWith projectWith f d = Array.filter (fst >> projectWith >> f) d |> Array.normalizeWeights

let inline conditionEntropyOn projectWith x d = conditionWith projectWith ((=) x) d |> entropy


let conditionalEntropy projectWith (joint:Distribution<_>) =
    Set.map projectWith joint.Support 
    |> Seq.sumBy (fun x -> 
          let p = probabilityOf (projectWith >> (=) x) joint
          let h = conditionEntropyOn projectWith x (pmf joint)
          p * h)

let mutualInformation (joint:Distribution<_>) =
    joint.Support |> Seq.sumBy (fun(x,y) ->
        let px = probabilityOf (fst >> (=) x) joint
        let py = probabilityOf (snd >> (=) y) joint 
        let pxy = probabilityOf ((=) (x,y)) joint
        pxy * log0(pxy/(px * py)))


let kldivergence (pA:Distribution<_>) (pB:Distribution<_>) =
    pA.Support |> Seq.sumBy (fun x ->
        let p_a = probabilityOf ((=) x) pA
        let p_b = probabilityOf ((=) x) pB
        p_a * log0(p_a/ p_b))

let kldivergence2 pA pB =
    pA |> Array.sumBy (fun ((x,_)) ->
        let p_a = probabilityOfWeighted2 ((=) x) pA
        let p_b = probabilityOfWeighted2 ((=) x) pB
        p_a * log0(p_a/ p_b))