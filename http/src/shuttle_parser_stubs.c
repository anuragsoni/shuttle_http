#include <caml/mlvalues.h>
#include <string.h>

CAMLprim value shuttle_parser_bytes_memchr_stub(value vba, value vba_off,
                                                value vchr, value vlen) {
  size_t off = Unsigned_long_val(vba_off);
  unsigned char *buf = Bytes_val(vba) + off;
  size_t len = Unsigned_long_val(vlen);
  int c = Int_val(vchr);

  unsigned char *res = memchr(buf, c, len);
  if (res == NULL) {
    return Val_long(-1);
  } else {
    return Val_long(off + res - buf);
  }
}

CAMLprim value shuttle_parser_bytes_memcmp_string(value vba, value vba_off,
                                                  value vstr, value vstr_off,
                                                  value vlen) {
  void *buf1 = ((char *)Bytes_val(vba)) + Unsigned_long_val(vba_off),
       *buf2 = ((char *)String_val(vstr)) + Unsigned_long_val(vstr_off);
  size_t len = Unsigned_long_val(vlen);

  int result = memcmp(buf1, buf2, len);
  return Val_int(result);
}
