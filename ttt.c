int main() {
  return ({ char x[3]; char (*y)[3]=x; y[0][0]=4; y[0][0]; });
}
