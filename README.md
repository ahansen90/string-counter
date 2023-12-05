# String Counter Webserver
This is a demo project of a simple webserver written in Haskell using the
Servant web framework.

## Building and executing
This project is built using cabal. Use `cabal build` to build the
application and `cabal exec string-counter` to start the server listening
on port 9000. It does use GHC 9 features, so you will need a recent
version of GHC. I tested it using GHC 9.4.7 and Cabal 3.10.2.0.

## Notes
* The application stores the strings and their counts in-memory while the server
is running, resulting in unbounded memory usage. A production implementation
should use an external tool (DB, redis, etc.) for caching/persisting the data to
avoid this issue.
* Concurrent modification is implemented using STM. This is perfectly fine for
many workloads, but may perform poorly under write contention. If high
throughput under high write contention is desirable, a different concurrency
implementation should be used. Reads are completely non-blocking, however.
* Being a demo, this lacks many of the features you would want in a production
system, such as logging, metrics, tracing, etc.

