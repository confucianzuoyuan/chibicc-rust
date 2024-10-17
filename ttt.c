int main() {
  return ({ struct {char a[3]; char b[5];} x; char *p=&x; x.b[0]=7; p[3]; });
}
