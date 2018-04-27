module GeneticQueen

open System

module String =
    let join (sep: string) (xs: #seq<string>) = String.Join(sep, xs)

module List =
    let rec remove i l =
        match i, l with
        | 0, _ :: xs -> xs
        | i, x :: xs -> x :: remove (i - 1) xs
        | _, [] -> failwith "index out of range"

    let rec splitAt i xs =
        match i, xs with
        | 0, xs -> ([], xs)
        | _, [] -> ([], [])
        | n, x :: xs -> 
            let l, r = splitAt (n - 1) xs
            x :: l, r

    let swap i j xs =
        if i = j then
            xs
        else
            let a, b = if i > j then j, i else i, j
            let initial = List.take a xs
            let middle = List.take (b-a-1) (List.skip (a+1) xs)
            let end_ = List.skip (b+1) xs
            let merge = [ yield! initial; yield List.item b xs; yield! middle; yield List.item a xs; yield! end_ ]
            merge

type Individual = {
    Positions: int list
}
with
    static member WithPositions positions = { Positions = positions }
    static member FromReproduction x y = { Positions = List.append x y }

type Couple = {
    X: Individual
    Y: Individual
}

type Population = {
    Individuals: Individual list
}
with
    static member WithIndividuals individuals = { Individuals = individuals }

type Config = {
    NumberOfQueen: int
    PopulationSize: int
    BirthRate: float
    MutationRate: float
    Random: Random
}

let produceChild (couple: Couple) numberOfQueen (random: Random) =
    let splitIndex = random.Next(0, numberOfQueen)
    let splitX = random.Next(0, 2) = 1
    let x, y =
        if splitX then
            List.splitAt splitIndex couple.X.Positions |> fst
            , List.splitAt splitIndex couple.Y.Positions|> snd
        else
            List.splitAt splitIndex couple.Y.Positions |> fst
            , List.splitAt splitIndex couple.X.Positions |> snd
    Individual.FromReproduction x y

let reproduce birthRate numberOfQueen (random: Random) couple =
    let probability = random.NextDouble()
    if probability <= birthRate then
        None
    else
        let child = produceChild couple numberOfQueen random
        Some child

let reproduction parents birthRate numberOfQueen (random: Random) =
    let rec loop couples = function 
        | [] -> couples
        | _ :: [] -> couples
        | x :: xs ->
            let i = random.Next(0, List.length xs)
            let y = List.item i xs
            let xs = List.remove i xs
            loop ({ X = x; Y = y } :: couples) xs
    let couples = loop [] parents.Individuals
    let children = 
        couples
        |> List.choose (reproduce birthRate numberOfQueen random)
    Population.WithIndividuals children

let getTwoDifferentRandom min max (random: Random) =
    let a = random.Next(min, max)
    let rec loop () =
        let b = random.Next(min, max)
        if a = b then
            loop ()
        else
            b
    let b = loop ()
    a, b

let produceMutant (child: Individual) numberOfQueen (random: Random) =
    let a, b = getTwoDifferentRandom 0 numberOfQueen random
    let positions = List.swap a b child.Positions
    { Positions = positions }

let mutate mutationRate numberOfQueen (random: Random) child =
    let probability = random.NextDouble()
    if probability <= mutationRate then
        None
    else
        let child = produceMutant child numberOfQueen random
        Some child

let mutation children mutationRate numberOfQueen (random: Random) =
    let mutants =
        children.Individuals
        |> List.choose (mutate mutationRate numberOfQueen random)
    Population.WithIndividuals mutants

let calculateConflicts individual =
    let indexed =
        individual.Positions
        |> List.mapi (fun i x -> i, x)
    indexed
    |> List.sumBy (fun (i, position) ->
        let calc f =
            indexed
            |> List.sumBy (fun (j, otherPosition) ->
                if i <= j then
                    0
                else
                    if position = f i j otherPosition then 1 else 0
            )
        let right = calc (fun _ _ otherPosition -> otherPosition)
        let up = calc (fun i j otherPosition -> otherPosition + (j - i))
        let down = calc (fun i j otherPosition -> otherPosition - (j - i))
        right + up + down
    )

let killOne population (random: Random) =
    let a, b = getTwoDifferentRandom 0 (List.length population.Individuals - 1) random
    let individuals = population.Individuals
    let individualA = List.item a individuals
    let individuals = 
        individuals
        |> List.remove a
    let individualB = List.item b individuals
    let population =
        individuals
        |> List.remove b
        |> Population.WithIndividuals
    let conflictA = calculateConflicts individualA
    let conflictB = calculateConflicts individualB
    let chosen =
        if conflictA = conflictB then
            let takeFirst = random.Next(0, 2) = 1
            if takeFirst then
                individualA
            else
                individualB
        elif conflictA < conflictB then
            individualA
        else
            individualB
    { population with Individuals = chosen :: population.Individuals }

let reducePopulation population populationSize (random: Random) =
    let rec loop population =
        if List.length population.Individuals <= populationSize then
            population
        else
            killOne population random
            |> loop
    loop population


let selection population populationSize random =
    let uniquePopulation = 
        List.distinct population.Individuals
        |> Population.WithIndividuals
    reducePopulation uniquePopulation populationSize random

let generation config (parents: Population) =
    let children = reproduction parents config.BirthRate config.NumberOfQueen config.Random
    let mutants = mutation children config.MutationRate config.NumberOfQueen config.Random
    let population = 
        [ parents.Individuals; children.Individuals; mutants.Individuals ] 
        |> List.collect id
        |> Population.WithIndividuals
    let parents = selection population config.PopulationSize config.Random
    parents

let run config population iteration =
    let rec loop population = function 
        | 0 -> population
        | iter ->
            let newPopulation = generation config population
            loop newPopulation (iter - 1)

    loop population iteration

let createIndividual numberOfQueen (random: Random) =
    List.init numberOfQueen (fun _ -> random.Next(0, numberOfQueen))
    |> Individual.WithPositions

let createPopulation numberOfQueen populationSize (random: Random) =
    List.init populationSize (fun _ -> createIndividual numberOfQueen random)
    |> Population.WithIndividuals

let printPopulation population =
    population.Individuals
    |> List.iter (fun individual ->
        let values =
            individual.Positions
            |> List.map string
            |> String.join ", "
        printfn "[%s], %d conflicts" values (calculateConflicts individual)
    )
    let average =
        population.Individuals
        |> List.averageBy (calculateConflicts >> float)
    printfn "Average conflicts: %.2f" average

[<EntryPoint>]
let main _ =
    let config = {
        NumberOfQueen = 8
        PopulationSize = 20
        BirthRate = 0.5
        MutationRate = 0.5
        Random = Random()
    }
    let iteration = 100
    let initialPopulation = createPopulation config.NumberOfQueen config.PopulationSize config.Random
    printPopulation initialPopulation
    let finalPopulation = run config initialPopulation iteration
    printPopulation finalPopulation
    Console.ReadKey () |> ignore
    0 // return an integer exit code
