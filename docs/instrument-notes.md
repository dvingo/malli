2021-12-19

What problem are you trying to solve?

I want to implement instrument for clojurescript functions.

Why? 
- replace guardrails will malli
Why?
- becuase I don't want to use specs but I do want to get dev-time function checking.
Why?
Because it helps you when developing.
Why?
Because it tells you when a contract is broken

Because the malli fn instrumenting is syntactically separate from the fn def it can be added later and can more easily 
be generated and also elimanted from production artifacts.

Make a plan:
1. gain understanding of instrument in clojure 
2. introduce same API for use in clojurescript
3. take inspiration from cljs.spec.test.alpha/instrument and instrument-1
4. Test it - browser, node.js

More fine-grained steps:
[] Start a clojure repl in the malli project
[] start a cljs repl in the malli project - check its current shadow-cljs.edn
[] document the pieces of malli instrument and how it works in clojure
[] document the steps to take to copy the implementation to cljs macros.

# General dev notes

goal: start a node.js repl 

- installed shadow-cljs (npm i -D shadow-cljs)
- copy the version of shadow-cljs into deps.edn
- start the shadow-cljs server: `./node_modules/.bin/shadow-cljs server`
- navigate to: http://localhost:9630/dashboard
