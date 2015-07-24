# sniper

A Clojure library designed to delete dead code, including an emacs mode.

See sniper.scope's namespace docstring for details for how to use it.

The analysis is currently far from perfect, but we've used it to successfully delete about 10% of our 160KLOC codebase.  Contributions welcome.

## Motivation and Features

Over the years we've accumulated lots of dead code mixed into our namespaces, and getting rid of it manually is a painful job.  At the same time, a fully automated solution won't work since there are lots of functions that aren't used that we want to keep.

My ideal workflow for this would be a tool-assisted, interactive loop, where:
 1. the tool identifies a form that appers to be unused (except by tests)
 2. the user decides to keep the form, or deletes it and does accompanying cleanup of the code
 
I couldn't find any tools that met these criteria (and worked on our 160KLOC codebase), so I wrote sniper.  

## How it works

See sniper.scope's namespace docstring for details, but the basic idea is: 

 1. You start a REPL that has all your code on the classpath
 2. Sniper uses clojure.tools.analyzer to analyze the code, and find definitions and references from each form
   - This phase still has some errors, which may lead to false positives or negatives later.
 3. You tell sniper using regexes which forms are "shadow" (a.k.a. supporting).  For example, a test is a supporting form, 
    since the presence of a test doesn't prevent code from being dead.  
 4. You tell sniper using regexes which forms are "strong" (a.k.a. used).  For example, you might include `#"-main$"` as 
    a strong regex.  
 5. Then, you enter an interactive loop that takes you through the above workflow.  An included emacs mode can jump to each targeted form to allow you to quickly ascertain whether to kill or keep, and act accordingly.
 
## Features

Sniper understands shadow (e.g., test) forms, which behave as weak references, and can also "garbage collect" dead code cycles which might appear live from a local perspective.  

When you delete a form, if there are dependants such as shadow forms or forms involved in a reference cycle, sniper walks you through removing this collatoral damage as well.  
 

## License

Copyright © 2015 Jason Wolfe

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
