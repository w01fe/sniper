# sniper

A Clojure library designed to help you find and delete dead code, including an emacs mode.

See the description below and sniper.scope's namespace docstring for details for how to use it.

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

 1. Start a REPL that has all your code on the classpath, in addition to sniper (leiningen `[w01fe/sniper "0.1.0"]`).

 1. Require `sniper.scope` and call `sniper.scope/start!` with appropriate arguments. For example, 
  
 ```clojure
   (require '[sniper.scope :as ss])
   (sniper.scope/start!
     "/Users/w01fe/my-repo"
     [#"-main"
      #"/\$"]
     #"/test/")
 ```
    starts an interactive session removing unused code from `/users/w01fe/my-repo`, where all vars/classes
    whose name matches `#"/-main"` or `#"/\$"` (e.g., fnhouse handlers) are considered "strong",
    and all forms with a `test` in their file path are marked as "shadow" (i.e., supporting).

   - First, `start!` uses clojure.tools.analyzer to analyze the code, and find definitions and references from each form.
     - This may take awhile the first time, but results are cached so it should be much faster if you call it again 
     - This phase still has some errors, which may lead to false positives or negatives later.
   - Then, it runs a graph analysis on the full set of forms to find candidates for removal.  In this process:
     - "strong" forms matching your regexes (or entries in the `.sniper-strong.clj` cache), as well as any forms 
       they depend on, are never considered as candidates for deletion.
     - "shadow" forms matching your file regex are not counted as references for the purpose of deciding if a 
       form is dead or alive.  however, `sniper` still tracks them so that, e.g. if you delete a form, it guides you
       through deleting its test as well.

 1. Then, you enter an interactive loop that takes you through the above workflow.  This is best experienced by loading the attached `resources/sniper.el` emacs mode and activating `M-x sniper-mode`, but can also be done at the repl:
   - Type `C-M-'` to jump to the current target (or call `(ss/aim!)` at the repl and navigate there manually).
   - Then either:
     - Delete the code, do any additional cleanup desired, then press `C-M-backspace` (or call `(ss/fired!)`) to 
       tell sniper about your decision.  If using `sniper.el`, it will immediately aim at the next target.
     - Decide to keep the form, and press `C-M-=` (or call `(ss/spare!)` to tell sniper about it.  This decision will be cached to the `.sniper-strong.clj` file, and sniper will aim at the next target.
   - Repeat until there is nothing left to remove.  Didn't that feel good?!
   - If at any point you want to pause, or something gets confused, you can always call `start!` again with the same arguments to pick back up.  Both the analysis and your decisions are cached on disk.
    

## Features

Sniper understands shadow (e.g., test) forms, which behave as weak references, and can also identify dead code cycles (a calls b, b calls a, nothing else calls either) which might appear live from a local perspective.  

When you delete a form, if there are dependents such as shadow forms or forms involved in a reference cycle, sniper walks you through removing this collateral damage as well.  
 
Sniper caches the results of analysis and the forms you mark as live, so while the first setup run on a project may be very slow (largely time in the analyzer), after making changes you can typically reanalyze a large codebase in seconds or less.  

## License

Copyright © 2015 Jason Wolfe

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
