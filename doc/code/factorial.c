int factorial(int n) {
  int f;
  f = 1;
  while(n > 0) {
    f *= n--;
  }
  return f;
}
