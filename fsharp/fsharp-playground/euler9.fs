module Euler.P9

(*
Special Pythagorean triplet
Problem 9

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*)

let triplet sum =
  let sequence = seq {
    for c in (sum - 3)..(-1)..1 do
    for b in (sum - c - 1)..(-1)..2 do
    for a in (sum - c - b)..(-1)..3 do
    if a * a + b * b = c * c && a + b + c = sum then 
      yield (a, b, c) 
  }
  
  sequence |> Seq.tryPick Some

let product (a, b, c) =
  a * b * c
  
let euler9() =
  triplet 1000 |> Option.get |> product