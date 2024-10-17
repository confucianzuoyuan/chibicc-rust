int main() {
  return ({ struct t {char a[2];}; { struct t {char a[4];}; } struct t y; sizeof(y); });
}
