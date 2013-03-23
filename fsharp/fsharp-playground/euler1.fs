module Euler.P1

(*
Multiples of 3 and 5
Problem 1

If we list all the natural numbers below 10 that are multiples of 3 or 5, we 
get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
*)

let euler1 until =
  seq { 1..until } 
  |> Seq.filter (fun it -> it % 3 = 0 || it % 5 = 0) 
  |> Seq.reduce (+)
    
let result = euler1 1000