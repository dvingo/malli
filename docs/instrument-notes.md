2021-12-20
in malli.core/=> and malli.core/-register-function-schema!
I had to change the implementation to only store the schema vector and then also execute it at macro-expansion time 
because we need the list of instrumented fn vars at macro time to implement this.
So then I'll need to invoke malli.core/function-schema at runtime so that it works in cljs

malli.instrument/-strument is the function that needs to be replaced by a macro in order to 
be used from cljs

Steps:
[] first try just calling malli.core/-instrument with a cljs function - all at runtime, no
macros
[] Then translate malli.instrument/-strument! to a cljs macro
find-var -> ana-api/resolve &env sym
alter-meta! ->  this exists as a function in cljs.core.cljs
alter-var-root -> set!

-------------------------

start a dev setup:
```bash
./node_modules/.bin/shadow-cljs watch play
```

clojure repl for macros:
in your tool's/editors equivalent:

```bash
clj -A:shadow
```

Add 

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
be generated and also eliminated from production artifacts.

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

in node-js the following fails:

```clojure 
(m/validate [:maybe string?] "kikka" )
```
but the following works:

```clojure 
(m/validate [:maybe :string] "kikka" )
```
I think there is an issue with the registry and equality for looking up functions in the registry on node.js.
Not sure.
ok, now it's working and I have no idea why...

--- 
I'm thinking there are some weird caching issues or something going on in the node.js target. 
Things break then randomly start working.


---- 

okay - I ran into the same thing with the browser build. I had to refresh the page and then
the schema calls started working.
I may next try chaning the caching options of shadow-cljs
https://shadow-cljs.github.io/docs/UsersGuide.html#_compiler_cache
`:build-options {:cache-level :jars}}`

and now I tried it again with the node.js target and ran into the same problem. I killed the node.js
process and then restarted it with the shadow-cljs cljs repl still open.
Then reloaded the file into the cljs REPL and the (m/schema) calls worked.


Steps:
1. [] Define a var in a cljs file
2. [] write a macro, in a clj file, to replace that var 
3. [] invoke that macro in the cljs file with the var in 1.
4. [] Evaluate the var defined in 1. - see it's value has changed via the macro.
5. [] Do the same operation for every var in a given namespace.
6. [] Do the same thing for every var in malli.core/-function-schemas*
