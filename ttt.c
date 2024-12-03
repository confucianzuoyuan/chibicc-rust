int add2(int x, int y) {
  return x + y;
}

int main() {
    return ({ int (*fn)(int,int) = add2; fn(2,5); });
}