int main() {
  // hello
  return ({ typedef struct {int a,b;} T; T x={1,2}; T y=x; y.a; });
}
