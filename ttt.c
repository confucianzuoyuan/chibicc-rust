/* Copyright (C) 1991-2024 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
void assert(int expected, int actual, char *code);
int printf();
int main() {
  assert(0, 0, "0");
  assert(42, 42, "42");
  assert(21, 5+20-4, "5+20-4");
  assert(41, 12 + 34 - 5, "12 + 34 - 5");
  assert(47, 5+6*7, "5+6*7");
  assert(15, 5*(9-6), "5*(9-6)");
  assert(4, (3+5)/2, "(3+5)/2");
  assert(10, -10+20, "-10+20");
  assert(10, - -10, "- -10");
  assert(10, - - +10, "- - +10");
  assert(0, 0==1, "0==1");
  assert(1, 42==42, "42==42");
  assert(1, 0!=1, "0!=1");
  assert(0, 42!=42, "42!=42");
  assert(1, 0<1, "0<1");
  assert(0, 1<1, "1<1");
  assert(0, 2<1, "2<1");
  assert(1, 0<=1, "0<=1");
  assert(1, 1<=1, "1<=1");
  assert(0, 2<=1, "2<=1");
  assert(1, 1>0, "1>0");
  assert(0, 1>1, "1>1");
  assert(0, 1>2, "1>2");
  assert(1, 1>=0, "1>=0");
  assert(1, 1>=1, "1>=1");
  assert(0, 1>=2, "1>=2");
  assert(0, 1073741824 * 100 / 100, "1073741824 * 100 / 100");
  printf("OK\n");
  return 0;
}
