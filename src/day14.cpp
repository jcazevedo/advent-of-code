#include <bits/stdc++.h>

using namespace std;

typedef long long ll;

struct Reaction {
  ll quantity;
  vector<tuple<ll, string>> ingredients;
};

ll gcd(ll a, ll b) {
  return b == 0 ? a : gcd(b, a % b);
}

map<string, Reaction> readInput() {
  ifstream fin("day14.in");
  string line;
  map<string, Reaction> res;
  while (getline(fin, line)) {
    replace(line.begin(), line.end(), ',', ' ');
    replace(line.begin(), line.end(), '=', ' ');
    replace(line.begin(), line.end(), '>', ' ');
    istringstream ss(line);
    ll q, g = 0; string i;
    Reaction r;
    while (ss >> q >> i) {
      r.ingredients.push_back({q, i});
      g = gcd(q, g);
    }
    tuple<ll, string> ing = r.ingredients.back(); r.ingredients.pop_back();
    r.quantity = get<0>(ing);
    res[get<1>(ing)] = r;
  }
  fin.close();
  return res;
}

ll getMinAux(ll quantity, string target, string from, const map<string, Reaction>& reactions, map<string, ll>& extra) {
  Reaction r = reactions.at(target);
  ll targetQ = quantity - extra[target];
  ll mult = (targetQ + (r.quantity - 1)) / r.quantity;
  extra[target] = (mult * r.quantity) - targetQ;
  ll cnt = 0;
  for (auto ingredient : r.ingredients) {
    if (get<1>(ingredient) == from) {
      cnt += (mult * get<0>(ingredient));
    } else {
      cnt += getMinAux(mult * get<0>(ingredient), get<1>(ingredient), from, reactions, extra);
    }
  }
  return cnt;
}

ll getMin(ll quantity, string target, string from, const map<string, Reaction>& reactions) {
  map<string, ll> extra;
  return getMinAux(quantity, target, from, reactions, extra);
}

ll getMax(string target, string from, ll maxV, const map<string, Reaction>& reactions) {
  ll lo = 0, hi = maxV, mid;
  while (lo < hi) {
    mid = lo + (hi - lo) / 2;
    if (getMin(mid, target, from, reactions) <= maxV)
      lo = mid + 1;
    else
      hi = mid;
  }
  return lo - 1;
}

int main() {
  map<string, Reaction> reactions = readInput();
  cout << "Part 1: " << getMin(1, "FUEL", "ORE", reactions) << endl;
  cout << "Part 2: " << getMax("FUEL", "ORE", 1000000000000ll, reactions) << endl;
  return 0;
}
