# Shuttle

Reasonably performant buffered channels<sup>[1](#channel)</sup> for async.

## Overview

There are multiple options for performing I/O using [async](https://github.com/janestreet/async_unix).
The Reader and Writer modules provide a high level API for buffered I/O, and are typically the go-to option for
applications that use async for I/O. These modules maintain their own internal buffer, and offer
users a simple interface that lets them interact with the underlying file descriptor without having
to write custom (and often error prone) buffer management logic.

The Reader/Writer modules provide convinence, but they might not be suitable in scenarios where one wants
slightly more control over when write operations are scheduled, or when the overhead of the reader/writer is not desirable<sup>[2](#overhead)</sup>.
In such scenarios, it is possible to drop down to async's [Fd](https://github.com/janestreet/async_unix/blob/4deb094dd60c22229f63b1e8467f0f7e0f18069d/src/fd.mli)
module and schedule reads and writes manually. This approach will give full control over how I/O operations are scheduled, but will come at a cost of more complexity
as the user is now responsible for buffer management, and proper handling of syscall responses.

Shuttle aims to provide a middle ground for a subset of use-cases handled by Reader/Writer. It covers fewer use-cases when
compared to Reader/Writer, only supports file descriptors that support non-blocking system calls, and doesn't aim to support the full
feature set of the reader/writes modules. In return, it allows for a high level api that pairs the ease of use of reader/writer modules with
performance and latency curves that are closer to implementations with manual buffer management.

#### Supported Platforms

Currently only linux, macOS and bsd based systems.

## How to Install

`opam install shuttle` for the current published release, and `opam pin add shuttle --dev-repo` for the development version.

*Notes*:
- <a name="channel">1</a>: Channel is a high-level construct for performing I/O. 
- <a name="overhead">2</a>: As always, make your own measurements. Also weigh in the fact that reader/writer modules in async are mature, battle tested and cover more use-cases.
- <a name="fork">3</a>: The implementation started as a fork [async_rpc's](https://github.com/janestreet/async/blob/7e71341ab2b962c56b98f293a3bec6098eafd1b0/async_rpc/src/rpc_transport_low_latency.ml) low latency transport.
