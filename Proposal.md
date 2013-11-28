# nREPL Self-describing Op Proposal

Implementing a self-describing nREPL op consists of writing a `defn`
with a map attached to its `:nrepl/op` metadata. Only one key is
required: a `:name` string, which is used to generate the name of the
corresponding command client side. It's strongly recommended that you
include a `:doc` string which will be used client-side to describe the
command corresponding to the op. This is distinct from regular Clojure
docstrings since those describe server-side semantics.

The other supported yet optional key is `:args`, which describes what
arguments the client should send when invoking the op. It should be a
list of lists. Each list includes at least the name of the argument as
a string in the first position. If the client can provide enhanced
input (completion, etc) for an argument, you can place a type string
in the second position of the list to indicate what kind of completion
should be offered. If an argument needs prompting, the prompt string
should be in the third position.

## Argument Types

* `string` - Enter a free-form string. This is the default.

* `ns` - The argument should be a namespace. Note that all nREPL
  messages contain the namespace they're sent from, so ops don't need
  to explicitly ask for a namespace unless it's different from the
  current namespace.

* `region` - a list of file name, start offset, and end offset of a
  selection.

* `var` - Any var.

* `file` - Any file.

* `position` - A list of the filename and a character offset as an integer.

* `list` - Choose from a list of predetermined strings. This list is
  in the fourth position of the argument vector.

* `eval` - Evaluate the form in the fourth position and use the return
  value as the argument specification.

## Responses

When an op is invoked, it can result in any number of response maps
being sent asynchronously via `clojure.tools.nrepl.transport/send`.
There are certain keys in response maps which clients should try to
support.

TODO: re-frame this in terms of content-type

* `message` - A one- or two-line string to be displayed to the user in
  a transient way.

* `text` - A longer string indicating textual content that should be
  displayed in its own window or pane.

* `url` - A URL to either display directly or open in a browser.

* `position` - A list of a filename and a character offset as an integer.

* `overlay` - An indicator to mark a given line with a certain
  color. Should be a list consisting of a color, a line number, and
  optionally a file. If no file is given, it defaults to the file from
  which the operation was invoked.

* `clear-overlays` - Remove all overlays currently shown. The value
  can be a file in which to remove overlays; if nil is given then it
  means use the file from which the operation was invoked.

* `reload` - Instructs the editor to reload a given file, assuming an
  edit has been performed server-side.

* `images` - PNG, JPG, SVG, you name it.

* `edit` - Perform edits on a file. TODO: details.

A single response map may contain any of these as well as standard
nREPL keys like `out`, `status`, etc.
