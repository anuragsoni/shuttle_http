open! Async

let set_nonblock fd = Fd.with_file_descr_exn fd ignore ~nonblocking:true
