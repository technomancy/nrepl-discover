# nREPL discovery

Proof-of-concept implementation for nREPL auto-discovered ops and
corresponding client-side auto-generated commands.

For background, see this thread: https://groups.google.com/group/clojure-tools/browse_thread/thread/c08b628a9af8346d

Currently implemented as an nrepl middleware with a reference
implementation of a toggle-trace nrepl operation using
[tools.trace](https://github.com/clojure/tools.trace).

With the `nrepl-discover.el` elisp included, it's possible to run `M-x
nrepl-discover` on an active nREPL session for this project, which
results in the creation of an `M-x nrepl-toggle-trace` Emacs
command. Invoking this command prompts for a var and uses
`tools.trace` to toggle tracing on the given var.

Other nREPL operations which take strings or no prompts and simply
return strings should be able to be implemented entirely server-side.

Lots to do:

* Solidify completion types to support for argument lists:
 * var
 * namespace
 * file
 * position
 * choose-one-of-n (find a good name for this)

* Write annotated nrepl ops for:
 * inspector (can we represent hypertext support this op needs?)
 * tools.namespace
 * ritz
 * clojure.test

* Figure out what response types to support beyond strings
 * URLs
 * overlays
 * file edits? (or just make the edit server-side and tell client to refresh)

* Determine whose responsibility it is to load namespaces containing nrepl ops so they can be discovered.

* Does any of this even make sense for vim or CCW?
