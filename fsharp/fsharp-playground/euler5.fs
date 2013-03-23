module Euler.P5

(*
Smallest multiple
Problem 5

2520 is the smallest number that can be divided by each of the numbers from 1 
to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the 
numbers from 1 to 20?
*)

let smallestEvenlyDivisbleUntil number =
  let divisors = seq { number..(-1)..2 }
  let isEvenlyDivisable num =
    divisors |> Seq.forall (fun divisor -> num % divisor = 0)
  
  let numbers = Seq.initInfinite (fun index -> number * (index + 2))
  numbers |> Seq.find isEvenlyDivisable
  
let euler5() = smallestEvenlyDivisbleUntil 20