# CS152 Assignemnt 1 - Cryptanalysis
__Student repository__

The problem statement is in the accompanying `.pdf`. This document is to inform
you concretely about what is expected in your submission and some tips on how to
navigate the various modules in here. We also detail how to make use of the
sample implementation.

Make sure you follow the crisp instructions below to help us automatically grade
your submission. **Most importantly do not commit plagiarism. See distinction
between plagiarism and discussion
[here](https://www.cse.iitb.ac.in/~supratik/courses/copying-and-discussion.html)**.

The assignment essentially requires you to implement `dictionary-closure`,
`secret-word-enumeration`, some statistical analysis functions, and a driver
function: `crack-cipher`.

# Instructions
1. EDIT:
   - main.rkt
   - dict-closure.rkt
   - statistics.rkt
   - strategies.rkt
2. DO NOT EDIT:
   - utils.rkt
3. `crack-cipher` in main.rkt should implicitly use `utils:ciphertext` and
   `utils:cipher-word-list`.
4. For the function skeletons we provide,
   - do not change the arguments,
   - add or reduce arguments,
   - remove them.
5. Make sure you take a look at the utilities in `utils.rkt`, before starting
   the assignment.
6. Each module has instructions or some text to guide you.
7. Keep your code nicely indented and documented, if you wish to get any partial
   credits. This requires conscious effort and takes up time - pace your work
   well!
8. You are free to add more modules to your submission. They must be implemented
   by you. You must `require` `utils.rkt` in your new module.
9. Make use of built-in racket functions as much as you can. Why re-invent the
   wheel? Getting good at reading docs is a valuable skill.
10. Detailed submission instructions will be intimated on Moodle later!

## FAQs
1. I can't wrap my head around the `prefix-in` business.
   - `require` imports all top-level functions. The `prefix-in` adds a prefix,
     to group them.
	 This allows you to define functions with the same name in different modules
     without name clashes. More importantly, it clearly indicates where this
     function was imported from, making your code more readable.
2. What should I start with?
   - We recommend that you begin with the statistics module and then a dumb
     `etai` strategy. At this point you can start working on `crack-cipher`.
   - Add in `dictionary-closure`, improving the strategy, and
     `secret-word-enumeration` incrementally. Don't try to do everything at
     once.
3. How will you grade my submission?
   - We'll replace `utils.rkt` with our own (which has strictly more functions
     than the one we've provided).
   - ~We won't run your `main.rkt`, just require `crack-cipher` from your
     `main.rkt` (and other functions of interest from resp. modules) into our
     test-script.~ 
	 
	 **Will be intimated later.**
4. Any tips for an additional module that I'm including in my submission?
   - Yes! See point 8 in "Instructions"
5. I think I found a bug in the starter kit or demo application.
   - Please report it on a Moodle thread.

# Using the sample implementation

# References
* Simon Singh's [supplementary
  website](http://www.simonsingh.net/The_Black_Chamber/chamberguide.html "The
  BLACK Chamber") with some hints on cryptanalysis.
* Another good source of strategies and generic approaches on [Practical
  Cryptography](http://practicalcryptography.com/cryptanalysis/).
* We've sourced words (ie, dictionary), and statistics from [Peter Norvig's
  works](http://norvig.com/mayzner.html).
* A [fun tool](https://www.guballa.de/vigenere-solver) on a different cipher
  scheme.

-----------------

Hope you enjoy doing this assignment as much as we did making it!

Best luck!
