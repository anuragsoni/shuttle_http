# Shuttle_http

Shuttle_http is a HTTP/1.1 implementation for OCaml that uses [async](https://opensource.janestreet.com/async/) to provide asynchronous servers and clients.

This is a relatively low-level library that attempts to provide building blocks for writing http servers and clients. The goal for this library is to be a building block for other libraries and applications.

## Getting Started

You can install the library using opam.

```sh
opam install shuttle_http
```

Once installed, you'll need to add `shuttle_http` as a dependency in your project's dune file. ex:

```scheme
(executable
  (name foo)
  (libraries shuttle_http))
```

### Getting Started with Servers

Shuttle_http is built on top of `Core` and `Async`. Core is intended to be used as a replacement of the OCaml standard library, and Async is a library that implements a non-preemptive user-level threads and provides a high level api for asynchronous execution. The rest of this doc assumed the following modules have been opened:

```ocaml
open! Core
open! Async
open! Shuttle_http
```

#### Defining a Service

A Service defines how a server responds to incoming requests. It is an asynchronous function that accepts a HTTP request and returns
a deferred HTTP Response.

```ocaml
let hello_service (_ : Request.t) =
  return (Response.create ~body:(Body.string "Hello World") `Ok)
;;
```

This service will respond to all requests with a 200 status code and a body with the content "Hello World". Shuttle_http will automatically populate the Content-Length header in the response.

#### Running a Server

We will need to launch a server that will accept `hello_service` and start a running TCP server.

```ocaml
let main port =
  let server =
    Server.run_inet (Tcp.Where_to_listen.of_port port) (fun _addr -> service)
  in
  Log.Global.info
    !"Server listening on: %s"
    (Socket.Address.to_string (Tcp.Server.listening_on_address server));
  Tcp.Server.close_finished_and_handlers_determined server
;;
```

To launch our server, we will leverage async's `Command.async`, which will use the `main` function we defined, start the Async scheduler before `main` is run, and will stop the scheduler once `main` returns.

```ocaml
let () =
  Command.async
    ~summary:"Start an echo server"
    (Command.Param.return (fun () -> main 8080))
  |> Command_unix.run
;;
```

#### Echo Server

Our `hello_service` doesn't really do much, we'll now see examples of servers that do a little more work than always responding with the same payload for every request. This example will show how to echo the body received in an incoming request back to the client. We'll also need to do some routing and since `shuttle_http` doesn't ship with a router we'll rely on pattern matching:

```ocaml
let service request =
  match Request.path request, Request.meth request with
  | "/echo", `POST -> return (Response.create ~body:(Request.body request) `Ok)
  | "/", `GET -> return (Response.create ~body:(Body.string "Hello World") `Ok)
  | ("/echo" | "/"), _ -> return (Response.create `Method_not_allowed)
  | _ -> return (Response.create `Not_found)
;;
```

This is a more involved service, we use pattern matching to dispatch our service on a combination of request path and http method. If we receive a `POST` request on the `/echo` path, we return a new response that uses the same body stream as the incoming request.
Shuttle_http will ensure that the incoming request body is streamed incrementally and echoed back out to the client.
