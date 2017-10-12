# tornyxorn

A server application providing a small api that estimates the difficulty of
attacking targets in [Torn](https://www.torn.com). Written in Clojure, backed
by Datomic.

## Post-Mortem

This was a failed experiment at rewriting
[difficulty-api](https://github.com/codonnell/difficulty-api) using a dataflow
programming style with `core.async`. The effort failed for a few reasons.

### (Negative) Swallowed Exceptions

The biggest problem is that without careful design, exceptions could crash a
thread and go unnoticed. The code here would need considerable refactoring to
ensure all exceptions are caught and at least logged. An attempt was made to
make up for this deficiency with thorough logging, but it was not enough.

### (Negative) Difficult to Test

The code is chock full of intermingled side effects. Side effects are a
necessary evil, but they need to be isolated. If most of the business logic
were implemented using pure functions, it would have been much easier to write
tests. It also would have been easier to handle exceptions, as their origins
could be more easily controlled.

### (Positive) Use of `component` Library

The reason it was possible to get this application working despite its
considerable flaws was its modular design. Because each component could be
started, passed messages, and stopped independently, it was easy to test each
component in the repl.

## Recommendations for Change

This application is not a great fit for a dataflow programming style.
Concurrency is hard, and adding it unnecessarily should be avoided. A better
solution would be to respond to registration and difficulty requests
synchronously. Any background update jobs could be run asynchronously, but
should be designed carefully to capture and handle any exceptions.

A function which calls to an external API should not also make a local side
effect, like updating the database. It would be preferable to have such a
function return data encapsulating any necessary updates. Then such a function
can be mocked for testing purposes, and any exception handling code knows that
its errors must come from interacting with the external service. The background
job can then validate the data and make any database updates separately.

## License

Copyright © 2016 Chris O'Donnell

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
