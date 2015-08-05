# sniper

A Clojure library designed to delete dead code, including an emacs mode.

See sniper.scope's namespace docstring for details for how to use it.

The analysis is currently far from perfect, but we've used it to successfully delete about 10% of our 160KLOC codebase.  Contributions welcome.

## Motivation

Over the years we've accumulated lots of dead code mixed into our namespaces, and getting rid of it manually is a painful job.  At the same time, a fully automated solution won't work since there are lots of functions that aren't used that we want to keep.

My ideal workflow for this would be a tool-assisted, interactive loop, where:
 1. the tool shows the user a form that appears to be unused (except possibly by tests)
 2. the user decides to keep the form and marks it as used, or deletes it and does accompanying cleanup of the code
 3. the tool updates its internal dependency graph based on the user action, and goes back to (1).
 
I couldn't find any tools that met these criteria (and worked on our 160KLOC codebase), so I wrote sniper.  

## How to use it

See sniper.scope's namespace docstring for details, but the basic workflow is: 

 1. Start a REPL that has all your code on the classpath, in addition to sniper (leiningen `[w01fe/sniper "0.1.0]`).
 1. Require `sniper.scope` and call `sniper.scope/start!`. 
 1. Sniper uses clojure.tools.analyzer to analyze the code, and find definitions and references from each form
   - This phase still has some errors, which may lead to false positives or negatives later.
 1. You tell sniper using regexes which forms are "shadow" (a.k.a. supporting).  For example, a test is a supporting form, 
    since the presence of a test doesn't prevent code from being dead.  
 1. You tell sniper using regexes which forms are "strong" (a.k.a. used).  For example, you might include `#"-main$"` as 
    a strong regex.  
 1. Then, you enter an interactive loop that takes you through the above workflow.  An included emacs mode can jump to each targeted form to allow you to quickly ascertain whether to kill or keep, and act accordingly.
 
## Features

Sniper understands shadow (e.g., test) forms, which behave as weak references, and can also identify dead code cycles (a calls b, b calls a, nothing else calls either) which might appear live from a local perspective.  

When you delete a form, if there are dependents such as shadow forms or forms involved in a reference cycle, sniper walks you through removing this collateral damage as well.  
 
Sniper caches the results of analysis and the forms you mark as live, so while the first setup run on a project may be very slow (largely time in the analyzer), after making changes you can typically reanalyze a large codebase in seconds or less.  

## License

Copyright © 2015 Jason Wolfe

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
