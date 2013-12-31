# nREPL discovery

Proof-of-concept design for declaring auto-discovered nREPL operations
and corresponding client-side auto-generated commands.

For background, see [this thread](https://groups.google.com/group/clojure-tools/browse_thread/thread/c08b628a9af8346d) and [this one too](https://groups.google.com/forum/#!topic/clojure-tools/rkmJ-5086RY).

An nREPL operation can be any piece of functionality performed
server-side that would be convenient to expose from the editor. For
example, you can run the tests you're editing by switching to the REPL
and calling `clojure.test/run-tests`, but it's more convenient to have
an editor command you can invoke directly and bind to whatever
keystroke you like.

See `src/nrepl/discover/samples.clj` for some samples of what
user-defined ops can do. As a proof-of-concept it includes a reference
implementation of a `toggle-trace` operation using
[tools.trace](https://github.com/clojure/tools.trace) as well as a
couple operations for running `clojure.test` tests.

There are really three distinct parts to this:

* A server-side nREPL middleware to find metadata all available
  operations and expose them to clients.
* A set of clients (currently only one) implementing a compiler of
  operation metadata -> editor commands.
* A predetermined vocabulary (see `Proposal.md`) for acceptable
  argument declarations and content-types for responses that nREPL
  operations may use and clients should support.

This project is still under rapid development and should not be
considered stable.

## Usage

Place this in your `:user` profile:

```clj
:dependencies [[nrepl-discover "0.1.0"]]
:repl-options {:nrepl-middleware [nrepl.discover/wrap-discover]
               :init (require 'nrepl.discover.samples)}
```

With the `nrepl-discover.el` elisp package, (installable from
Marmalade) it's possible to run `M-x nrepl-discover` on an active
nREPL session for this project, which results in the creation of
commands for every loaded var which has `:nrepl/op` metadata attached.

In order to handle the response values, you currently need to run from
the `content-types` branch of
[Cider](https://github.com/clojure-emacs/cider).

For Emacs usage you would typically invoke `nrepl-discover` from
`nrepl-connected-hook` and define key bindings for
`nrepl-toggle-trace` and `nrepl-run-tests`, the latter of which should
be able to obsolete the `clojure-test-mode` library. The Emacs lisp
code is included as a reference implementation; the hope is that the
same functionality could be ported to other editors like Vim and
CounterClockwise.

# Lots to do

* Could this be used to implement completion? (need a new content-type?)

* Replace as many nrepl.el commands as possible
 * `unload-ns`
 * `macroexpand-1` / `macroexpand-all`
 * `doc`
 * `javadoc`
 * `clojuredocs`

* Implement for other editors

## License

Copyright © 2013 Phil Hagelberg and contributors

Emacs Lisp code distributed under the GNU General Public License,
either version 3 or (at your option) any later version.

Everything else distributed under the Eclipse Public License, the same as Clojure.
