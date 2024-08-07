# 0.12.0

* Adapt to Janestreet's 0.17 series.

# 0.11.0

* Forward a server context object to all http services. This can be used to lookup peer-socket address and ssl details (if using ssl) for the underlying connection.
* Add support for websockets

# 0.10.1

* Accept a `(string * string) list` as http headers.
* Add a `Body.to_string` for reading entire bodies as string.

# 0.10.0

* Adapt to changes in async_kernel 0.16
* Only support OCaml 4.14 or newer
* Only support HTTP codec in the library

# 0.9.4

* Allow setting an upper bound on buffer size.

# 0.9.2

* Allow creating ssl encrypted http servers.

# 0.9.1

* Add client that supports keep-alive.
* Add http clients that leverage Async_kernel's persistent connections.

# 0.9.0

* Forward ssl connection details to shuttle_ssl server handlers
* Remove listen and with_connection from shuttle_ssl. Use upgrade_server_connection and upgrade_client_connection with the regular Connection module provided within Shuttle.
* Allow accessing the underlying buffer capacity for channels
* Add helper method that creates server connections using inet sockets
* Reliably schedule stream cleanup for all streaming server responses without forcing users to use any special functions to create the response
* Add one-shot http client
* Setup tcp server within http server module

# 0.8.1

* Same content as 0.8.0, but it re-works the integration tests to not spawn external processes, and adds a constraint on shuttle_http to only have it available on 64 bit systems.

# 0.8.0
* Revive the http codec as a new shuttle_http package
  - Http codec supports a timeout for reading Request headers
  - Server module reports a deferred that resolves when the server context closes. This can be usedul to register cleanup actions that should run when a server connection is torn down.
  - Using the utility methods within the Server module to create responses ensures that streams are torn down if the server connection is closed before a stream was fully consumed.

# 0.7.0
* Remove support for blocking file descriptors
* Output_channel accepts an optional user-provided Async Time_source
* Input_channel accepts an optional user-provided Async Time_source
* Support timeouts for Input_channel.refill
* Remove `read`, `read_line`, `lines` from Input_channel

# 0.6.0
* Increase upper bound for core/async to 0.15.0

# 0.5.0
* Remove Buffer_is_full in favor of Bytebuffers that can grow upto a user provided max size
* Flush operations reports if the write operations encountered an error
* Reliably wakeup pending flushes when there is an error encountered while flushing pending bytes

# 0.4.0
* Remove Bytebuffer from public api
* Deprecate `schedule_bigstring`, `write_string`
* Support reading lines from an input channel
* Use Core_unix.IOVec to represent a view inside the input channel
* Support file descriptors that don't support nonblocking IO
* Remove the `read_one_chunk_at_a_time` interface from input channel
* Switch to 0.15 series of core and async

# 0.3.1
* Add support for using format strings for writing to an output channel.
* Remove support for deferred responses from chunked reader callbacks.
* Add a new `shuttle_http` library that implements a driver for httpaf server_connection.

# 0.3.0
* Support creating a reader pipe from `Input_channel`.
* Support creating a writer pipe from `Output_channel`.
* Support encrypted channels using `async_ssl`.

# 0.2.0

* Support deferred's in `Input_channel.read_one_chunk_at_a_time`
* Support writing characters to output channels

# 0.1.0

* Initial release
