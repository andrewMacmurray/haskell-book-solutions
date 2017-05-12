# Haskell Book Solutions

Solutions to exercises in [`haskell book`](http://haskellbook.com/), a fantastic resource for learning Haskell from scratch.

The project is built using [`stack`](https://docs.haskellstack.org/en/stable/README/) a tool used to build Haskell projects and manage dependencies. Check out https://docs.haskellstack.org/en/stable/README/ for information on downloading `stack`. (there's also a great video tutorial [here](https://www.youtube.com/watch?v=sRonIB8ZStw) to get haskell up and running with stack).

### Running code locally

After `stack` and `ghc` have been installed

1. clone the repo, cd into the directory
2. run `stack build`, this will install all the dependencies (can take a while)
3. run `stack ghci` to enter a `ghci` repl session
4. to load a particular module in `ghci` run `:l /path/to/the/module.hs`
5. you can then try out the functions by typing them into the repl
