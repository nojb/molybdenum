/// <summary>
/// Lab 1 Compiler: Positional Markers
/// </summary>
/// <remarks>
/// Author: Ben Hamme (bhamme)
/// Author: Adam Mihalcin (amihalci)
/// 15-411 Fall 2012
///
/// This file is intended to replace mark.sml from the SML starter code.
/// </remarks>

#light

namespace Lab1.Util

open System

module Mark =
    type ext = (int * int) * (int * int) * string

    let pos (row, col) =
        match col with
        | 0 -> sprintf "%d" row
        | _ -> sprintf "%d.%d" row col

    let show (l, r, file) =
        sprintf "%s:%s-%s" file (pos l) (pos r)

    type 'a marked = 'a * ext option

    let mark  d e = (d, Some e)
    let mark' d e = (d, e)
    let naked d = (d, None)

    let data = fst
    let ext = snd

    let extmin ((l1, c1 : int), (l2, c2 : int)) =
        if l1 < l2 then (l1, c1)
        elif l1 > l2 then (l2, c2)
        else (l1, Math.Min(c1, c2))

    let extmax ((l1, c1 : int), (l2, c2 : int)) =
        if l1 > l2 then (l1, c1)
        elif l1 < l2 then (l2, c2)
        else (l1, Math.Max(c1, c2))

    let rec wrap l =
        match l with
        | [] -> None
        | [e] -> e
        | e::el ->
            match wrap el with
            | None -> None
            | Some(el1, el2, elf) ->
                match e with
                | Some(e1, e2, ef) ->
                    if ef = elf then
                        Some(extmin(e1, el1), extmax(e2, el2), ef)
                    else
                        None
                | None -> Some(el1, el2, elf)

    let map f (d, e) = (f d, e)
    let map' f (d, e) = (f(d, e), e)

