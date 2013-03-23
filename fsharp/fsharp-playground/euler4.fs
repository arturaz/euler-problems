module Euler.P4

(*
Largest palindrome product
Problem 4

A palindromic number reads the same both ways. The largest palindrome made from
the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*)

let isPalindrome x =
  let str = x.ToString()
  let len = str.Length
  
  seq { 0..(len / 2) }
  |> Seq.forall (fun index -> str.[index] = str.[len - 1 - index])

let euler4() =
  seq { 
    for i in 999..-1..100 do
    for j in 999..-1..100 do
    yield i * j
  }
  |> Seq.find isPalindrome