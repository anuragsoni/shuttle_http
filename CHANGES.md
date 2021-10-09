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
