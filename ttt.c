int main() {
  // hello
  return ({ union { int a; char b[4]; } x={0x01020304}; x.b[0]; });
}
