#include <bits/stdc++.h>

using namespace std;

typedef long long ll;

#define MAX_OPTS 1000

#define STACK 0
#define CUT 1
#define DEAL 2

#define N_CARDS 10007
#define N_CARDS_L 119315717514047ll
#define TIMES 101741582076661ll

int opts[MAX_OPTS], args[MAX_OPTS], N;

ll modAdd(ll a, ll b, ll mod) {
  return (a + b) % mod;
}

ll modSub(ll a, ll b, ll mod) {
  return (a + mod - b) % mod;
}

ll modMult(ll a, ll b, ll c) {
  ll x = 0, y = a % c;
  for ( ; b > 0; y = (y * 2) % c, b >>= 1)
    if (b & 1)
      x = (x + y) % c;
  return x % c;
}

ll modPow(ll a, ll b, ll c) {
  ll x = 1, y = a;
  for ( ; b > 0; y = modMult(y, y, c), b >>= 1)
    if (b & 1)
      x = modMult(x, y, c);
  return x % c;
}

ll modInv(ll n, ll mod) {
  ll b0 = mod, t, q;
  ll x0 = 0, x1 = 1;
  if (mod == 1)
    return 1;
  while (n > 1) {
    q = n / mod;
    t = mod, mod = n % mod, n = t;
    t = x0, x0 = x1 - q * x0, x1 = t;
  }
  if (x1 < 0)
    x1 += b0;
  return x1;
}

ll stackIdx(ll orig, ll N) {
  return N - orig - 1;
}

ll cutIdx(ll orig, ll N, ll cut) {
  if (cut < 0)
    return cutIdx(orig, N, N + cut);
  ll diff = N - cut;
  return (orig + diff) % N;
}

ll dealIdx(ll orig, ll N, ll inc) {
  return (orig * inc) % N;
}

ll getFinalIdx(ll pos, ll cards) {
  for (int i = 0; i < N; ++i) {
    switch (opts[i]) {
      case STACK:
        pos = stackIdx(pos, cards);
        break;
      case CUT:
        pos = cutIdx(pos, cards, args[i]);
        break;
      case DEAL:
        pos = dealIdx(pos, cards, args[i]);
        break;
    }
  }
  return pos;
}

int main() {
  ifstream fin("day22.in");
  string str;
  N = 0;
  while (getline(fin, str)) {
    if (str.rfind("deal into new stack", 0) == 0) {
      opts[N++] = STACK;
    } else if (str.rfind("deal with increment", 0) == 0) {
      istringstream ss(str);
      string tmp;
      int inc;
      ss >> tmp >> tmp >> tmp >> inc;
      opts[N] = DEAL;
      args[N++] = inc;
    } else if (str.rfind("cut", 0) == 0) {
      istringstream ss(str);
      string tmp;
      int inc;
      ss >> tmp >> inc;
      opts[N] = CUT;
      args[N++] = inc;
    }
  }
  cout << "Part 1: " << getFinalIdx(2019, N_CARDS) << endl;
  // f(x) = a * x + b
  // f^n(x) = a^n * x + b * sum(i = 0, i = n - 1)(a^i + b)
  // f^-n(x) = a^(-n) * x - b * (1 - a^(-n)) / (a - 1)
  ll targetIdx = 2020;
  // b = f(0)
  ll b = getFinalIdx(0, N_CARDS_L);
  // a = f(1) - b
  ll a = modSub(getFinalIdx(1, N_CARDS_L), b, N_CARDS_L);
  // aInv = 1/a
  ll aInv = modInv(a, N_CARDS_L);
  // a1Inv = 1/(a - 1)
  ll a1Inv = modInv(a - 1, N_CARDS_L);
  // product = a^(-n) * x = (1/a)^n * x
  ll product = modMult(modPow(aInv, TIMES, N_CARDS_L), targetIdx, N_CARDS_L);
  // sub = b * (1 - a^(-n)) / (a - 1) = b * (1 - (1/a)^n) * (1/(a-1)) = b * (1 - aInv^n) * a1Inv
  ll sub = modMult(modMult(b, modSub(1, modPow(aInv, TIMES, N_CARDS_L), N_CARDS_L), N_CARDS_L), a1Inv, N_CARDS_L);
  // res = a^(-n) * x - b * (1 - a^(-n)) / (a - 1) = product - sub
  ll res = modSub(product, sub, N_CARDS_L);
  cout << "Part 2: " << res << endl;
  return 0;
}
