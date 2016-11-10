/// <summary>
/// Lab 1 Compiler: Symbol tables
/// </summary>
/// <remarks>
/// Author: Ben Hamme (bhamme)
/// Author: Adam Mihalcin (amihalci)
/// 15-411 Fall 2012
///
/// This file is intended to replace symbol.sml from the SML starter code.
/// </remarks>

#light

namespace Lab1.Util

open Microsoft.FSharp.Collections
open System.Collections.Generic
open System

module Symbol =
    
    type symbol = string * int

    let bogus = ("?", -1)
    let isBogus t =
        match t with
        | (_, -1) -> true
        | _ -> false

    let compare ((n, i), (n', i')) =
        if i < 0 || i' < 0 then Order.Greater
        else Order.compareToOrder ( fun (a,b) -> a - b ) i i'

    let nexts = ref 0
    let initht () = new Dictionary< string , int > (128)
    let ht = ref (initht ())

    let reset () = (nexts := 0; ht := initht ())

    let symbol name =
        match (!ht).TryGetValue(name) with
        | ( true , i ) -> (name, i)
        | ( false , _ ) -> 
            let i = !nexts
            nexts := !nexts + 1
            (!ht).Add(name, i)
            (name, i)

    let name (n, i) = n

    type 'a table = Map<symbol,'a>

    let empty = Map.empty
    let digest l = List.foldBack ( fun (s, v) m -> Map.add s v m) l empty

    let bind t (s, x) = Map.add s x t
    let look t s = Map.tryFind s t
    let look' t s = Option.get (look t s)
    let count (t : Map<'a,'b>) = t.Count
    let elems t = List.map ( fun (_,c) -> c ) (Map.toList t)
    let elemsi t = Map.toList t
    let keys t = List.map ( fun (k,_) -> k ) (Map.toList t)
    
    let rec delimit' l s =
        match l with
        | [] -> s
        | [x] -> s + x
        | (x :: xs) -> delimit' xs (s + x + ", ")

    let delimit l = (delimit' l "[") + "]"

    type set = Set<symbol>

    let nullSet = Set.empty
    let singleton = Set.singleton
    let add S s = Set.add s S
    let remove S s = Set.remove s S
    let isMember S s = Set.contains s S
    let showmems S = delimit (List.map name ( Set.toList S ))

