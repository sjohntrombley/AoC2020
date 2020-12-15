#Day 13

You're trying to catch a bus. There's a timestamp that increments continuiously. Busses have an ID such that the bus arrives at your stop when `mod t id == 0` where `t` is the timestamp. The input has two lines. The first line is an integer, the second line is a list of busses, separated by commas, where each bus is either an integer bus ID or `x`.

## Part 1

Part 1 assumes the first line is the earliest timestamp you can catch the bus and the `x`'s in the second line are bus routes that aren't currently running. It asks for the ID of the first bus you can catch multiplied by the number of minutes you'll need to wait for that bus. The correct bus can easily be found using `minimumBy` and modular division.

## Part 2

Part 2 assumes the second line of the input is a pattern of bus arrival. You're supposed to find the first timestamp such that the first bus ID leaves at that timestamp and each other bus ID in the list arrives at a timestamp corresponding to its position in the list. Any position with an `x` doesn't require any busses to arrive that minute. Extra busses arriving along side a required bus aren't a problem, nor are any busses that arrive during an `x`.

Each bus is a constraint on `t`, namely bus `a` arriving `b` minutes after `t` means `mod t a == t-b`. If `mod t a == t-b` and `mod t c == t-d` (with `a` < `c`) then `mod t (a*c) == mod (n*a-b) (a*c)` where `n = rem (i*(b-d)) (c)` and `mod (i*a) c == 1` (`i` is the inverse of `a` (mod `c`)). The extended Euclidean algorithm can be used to find `i`. After that it's just a `fold`.

For added fun, some of the intermediate calculations overflowed the `Int`s, but switching to `Integers` fixed everything.
