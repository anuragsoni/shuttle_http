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

API Documentation can be viewed online on the [OCaml package registry](https://ocaml.org/p/shuttle_http/0.9.1/doc/index.html).

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

### Getting Started with Clients

We'll use `httpbin.org` has a target for all the examples related to HTTP clients. We'll need to create a new `address` entity that points to httpbin:

```ocaml
let httpbin_address =
  Client.Address.of_host_and_port (Host_and_port.create ~host:"httpbin.org" ~port:443)
;;
```

If the incoming response's body fits entirely in the client's buffer Shuttle_http will represent the body as a fixed sized string, otherwise the body is read as an asynchronous stream so the response can be processed without having to wait for the entire body to arrive over the write.

We'll write a utility function that will convert a streaming response body to a string:

```ocaml
let response_body_to_string response =
  let stream_to_string stream =
    let buffer = Buffer.create 128 in
    let%map () =
      Body.Stream.iter stream ~f:(fun chunk ->
        Buffer.add_string buffer chunk;
        Deferred.unit)
    in
    Buffer.contents buffer
  in
  match Response.body response with
  | Body.Empty -> return ""
  | Body.Fixed str -> return str
  | Body.Stream stream -> stream_to_string stream
;;
```

Shuttle_http offers a few different flavors of HTTP clients. The first one we'll see is a OneShot client. OneShot clients open a new TCP
connection, send a HTTP Request, wait to receive a Response and then shut-down the TCP connection once the entire response has been consumed.

#### Oneshot clients

```ocaml
let one_shot_client () =
  let%bind response =
    Client.Oneshot.call
      ~ssl:(Client.Ssl_config.create ())
      httpbin_address
      (Request.create `GET "/get")
  in
  printf "Response status: %d\n" (Response.status response |> Status.to_int);
  let%map body = response_body_to_string response in
  print_endline body
;;
```

This client sends a request to `httpbin` using a TLS encrypted connection, and logs the response.

#### Clients supporting keep-alive

```ocaml
let persistent_client () =
  let%bind httpbin =
    Deferred.Or_error.ok_exn (Client.create ~ssl:(Client.Ssl_config.create ()) httpbin_address)
  in
  Monitor.protect
    ~finally:(fun () -> Client.close httpbin)
    (fun () ->
      let%bind response = Client.call httpbin (Request.create `GET "/stream/20") in
      printf !"Headers: %{sexp: (string * string) list}" (Response.headers response);
      let%bind () =
        Body.Stream.iter_without_pushback
          (Body.to_stream (Response.body response))
          ~f:(fun chunk -> printf "%s" chunk)
      in
      let%bind response = Client.call httpbin (Request.create `GET "/get") in
      printf !"Headers: %{sexp: (string * string) list}" (Response.headers response);
      Body.Stream.iter_without_pushback
        (Body.to_stream (Response.body response))
        ~f:(fun chunk -> printf "%s" chunk))
;;
```

This example uses a client that supports keep-alive. The client object needs to be forward to every `Client.call` as it maintains internal state to ensure that the same tcp connection will be re-used for multiple requests. The client only send a new request once the previous response has been fully consumed.

Persistent clients are nice as they avoid paying the price of establishing a new TCP connection for subsequent requests. The drawback is that users need to be remember to close the client once they are done with it to avoid leaking file handles. `Monitor.protect` can be a good option
when using persistent clients as it'll provide a consistent cleanup stage via its `finally` callback which can be used to close the client object.
