int (*fnptr(int (*fn)(int n, ...)))(int, ...) {
  return fn;
}