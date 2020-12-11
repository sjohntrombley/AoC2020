# Day 10

## Part 1

Part 1 was trivial. Find the differences between adjacent elements then count
1's and 3's.

## Part 2

Initially, I tried to brute force it (e.g. `combinations (a:b:c:l) = if c-a < 4
then combinations (b:c:l) + combinations (a:c:l) else combinations (b:c:l)`),
but that took forever to run. I broke up the problem by splitting the input
into sublists that consist exclusively of consecutive adapters that can be
removed individually. Each of these sublists can be easily brute-forced, and
the product of the number of combinations for each sublist is the total number
of combinations. It's not optimal (worst case is still O(2^n)), but it works.
