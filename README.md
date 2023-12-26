# Advent of Code 2023
This repository contains my Advent of Code 2023 solutions, written in Haskell. This was my second year finishing Advent of Code, and my first year fully completing it in Haskell ([I used C++ in 2021](https://github.com/BenBean18/AoC2021), [and got close with Haskell in 2022](https://github.com/BenBean18/AoC2022)). I've enjoyed using Haskell and am a lot more experienced at it than I was at the beginning of AoC 2022. It's a fun language and a different way of thinking than imperative programming (saying what things *are*, not what the program should do). Also, you can write *really* nice and concise code sometimes (e.g. from day 9: `zipWith (-) (tail ints) (init ints)` to find the differences between numbers in a list).

```
===============================================================================
 Language            Files        Lines         Code     Comments       Blanks
===============================================================================
 Haskell                39         5424         3469          953         1002
===============================================================================
 Total                  39         5424         3469          953         1002
===============================================================================
```

## Notes/Thoughts/Reflections
- I did use Copilot for Day 1 (trying for speed), but then turned it off for the rest of the days
- I got the same global rank for part 1 and 2 (2971) on Day 2
- Day 6 had a fun analytical solution and it's FAST (<16us for both parts combined)
- I tried running a linear regression on Day 9 (see [src/Day9_old.hs](src/Day9_old.hs)), it didn't work, so then I just did it the way stated in the problem which worked /shrug
- Day 11 went well for me:
 - transposing the input to not have to handle rows/columns separately (I reused this on Day 13 and 14, there were lots of 2D arrays)
 - recognizing the "trick" behind Part 2 relatively quickly
- Day 15 was cool, it was interesting to implement a hash map by hand
- On Day 17, I recognized how to compress the search space of paths by only considering junction points (which I reused for Day 23)
- On Day 18, I finally got to use the shoelace theorem which I learned about on Day 10's reddit page :)
- Day 19 Part 2 took a WHILE and I was so happy when I finally solved it ("IT WORKFSDKOFJDOIGHIDODFVHOSDHV)PUE@)!^$)R@&R_@FP)H")
- Day 20 Part 1 was fun to implement
- Day 22 went pretty well, I recognized that the blocks could be traversed from lowest to highest. (also wrote some of the code on my phone and used iSH, an x86 emulator for iOS, to push to GitHub)
- Optimizing Day 23 was HARD...I learned a lot about the limitations of graph algorithms but figured it out after briefly looking at reddit and remembering what I did for Day 17
- Day 24 Part 1: I FINALLY GOT TO USE LINEAR ALGEBRA!!!
- Day 24 Part 2: WolframAlpha is cool AND I want to eventually find the closed form solution to use more linear algebra :)
- Day 25: my brute forcer finished after 3 hours as I was working on a better implementation...go figure. I also learned about Karger's Algorithm which was cool. I want to study more graph theory -- it looks really hard but super interesting.

## Other Useful Stuff
- Benchmarking data (obtained using the `criterion` library and my Intel Core i7-1165G7) can be found in the `.html` files in the root folder. (note: there isn't one for Day 24 because it requires clicking on a WolframAlpha link, and Day 25 uses Karger's algorithm which relies on random numbers so benchmarking is relatively unpredictable)

## Usage
- You'll need Haskell and Cabal and a ton of dependencies (...maybe I'll use C next year) which Cabal will install for you
- Inputs should be placed in a folder labeled with that day's name (e.g. `day1/input.txt`, `day18/input.txt`, etc. the day 1 sample input is there as an example)
- To run Part 1 code: `cabal run -O2 AoC2023 [day number] 1`
- To run Part 2 code: `cabal run -O2 AoC2023 [day number] 2`
- To benchmark a day: `cabal run -O2 AoC2023 [day number] b`