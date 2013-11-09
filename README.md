# nREPL discovery

System for declaring auto-discovered nREPL operations and
corresponding client-side auto-generated commands.

For background, see [this thread](https://groups.google.com/group/clojure-tools/browse_thread/thread/c08b628a9af8346d).

Currently implemented as an nrepl middleware with a reference
implementation of a toggle-trace nrepl operation using
[tools.trace](https://github.com/clojure/tools.trace) as well as an
operation for running `clojure.test` tests.

Place this in your `:user` profile:

```clj
:dependencies [[nrepl-discover "0.1.0"]]
:repl-options {:nrepl-middleware [nrepl.discover/wrap-discover]}
```

With the `nrepl-discover.el` elisp included, it's possible to run `M-x
nrepl-discover` on an active nREPL session for this project, which
results in the creation of commands for every var which has
`:nrepl/op` metadata attached.

There are a few sample ops including `toggle-trace` and `run-tests`
which invoke `tools.trace` and `clojure.test` functionality
respectively.

For Emacs usage you would typically invoke `nrepl-discover` from
`nrepl-connected-hook` and define key bindings for
`nrepl-toggle-trace` and `nrepl-run-tests`, the latter of which should
be able to obsolete the `clojure-test-mode` library. The Emacs lisp
code is included as a reference implementation; the hope is that the
same functionality could be ported to other editors like Vim and
CounterClockwise.

# Lots to do

* How would completion work with this?

* What does the inspector need? (html? how do we represent hyperlink targets?)

* Implement for other editors

* Unload-ns op

* Replace as many nrepl.el commands as possible
 * `jump-to-definition`
 * `macroexpand-1` / `macroexpand-all`
 * `doc`
 * `javadoc`

## License

Copyright © 2013 Phil Hagelberg and contributors

Emacs Lisp code distributed under the GNU General Public License,
either version 3 or (at your option) any later version.

Everything else distributed under the Eclipse Public License, the same as Clojure.
