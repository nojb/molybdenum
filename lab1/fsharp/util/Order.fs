/// <summary>
/// Lab 1 Compiler: Implementation of order types for F#
/// </summary>
/// <remarks>
/// Author: Ben Hamme (bhamme)
/// Author: Adam Mihalcin (amihalci)
/// 15-411 Fall 2012
/// </remarks>

#light

namespace Lab1.Util

open System

module Order =
    type order = | Greater | Less | Equal

    let compareToZero n =
        if n < 0 then Less
        elif n > 0 then Greater
        else Equal

    let compareToOrder fn = fun a b -> compareToZero(fn(a, b))

