#include <caml/mlvalues.h>
#include <errno.h>
#include <unistd.h>

CAMLprim value shuttle_stubs_unix_read_assume_nonblocking(value fd, value pos,
                                                          value buffer_length,
                                                          value buffer) {
  unsigned char *bstr = Bytes_val(buffer) + Long_val(pos);
  size_t len = Long_val(buffer_length);
  ssize_t count = read(Int_val(fd), bstr, len);
  if (count == -1) {
    return Val_long(-errno);
  } else {
    return Val_long(count);
  }
}
