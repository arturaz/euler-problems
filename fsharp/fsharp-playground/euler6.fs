module Euler.P6

(*
Sum square difference
Problem 6

The sum of the squares of the first ten natural numbers is,
12 + 22 + ... + 102 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)2 = 552 = 3025

Hence the difference between the sum of the squares of the first ten natural 
numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred 
natural numbers and the square of the sum.
*)

let sumOfSquares sequence =
  sequence |> Seq.fold (fun sum num -> sum + num * num) 0
  
let squareOfSum sequence =
  let sum = sequence |> Seq.reduce (+)
  sum * sum
  
let difference sequence =
  (squareOfSum sequence) - (sumOfSquares sequence)

let euler6() =
  difference <| seq { 1..100 }