int strcmp(char *p, char *q);

char g17[] = "foobar";
char *g22 = &g17-3;
int main() {
  // hello
  return strcmp(g22+3, "foobar");
}
