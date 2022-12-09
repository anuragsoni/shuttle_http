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
