typedef struct { char a, b[]; } T65;
T65 g65 = {'f','o','o',0};
int main() {
  // hello
  return sizeof(g65);
}
