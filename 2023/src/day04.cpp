#include <algorithm>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <unordered_map>
#include <vector>

using namespace std;

struct Card {
  int number;
  set<int> winning;
  set<int> youHave;
};

int matches(const Card& card) {
  vector<int> v(100);
  vector<int>::iterator it;
  it = set_intersection(card.winning.begin(), card.winning.end(),
                        card.youHave.begin(), card.youHave.end(), v.begin());
  v.resize(it - v.begin());
  return v.size();
}

int part1(const vector<Card>& cards) {
  int ans = 0;
  for (const Card& card : cards) {
    int m = matches(card);
    if (m > 0) { ans += 1 << (m - 1); }
  }
  return ans;
}

int total(int idx, const vector<Card>& cards, unordered_map<int, int>& cache) {
  if (cache.count(cards[idx].number)) { return cache[cards[idx].number]; }
  int ans = 1;
  int m = matches(cards[idx]);
  for (int next = idx + 1; next <= idx + m; ++next) {
    ans += total(next, cards, cache);
  }
  cache[cards[idx].number] = ans;
  return ans;
}

int part2(const vector<Card>& cards) {
  int N = cards.size();
  unordered_map<int, int> cache;
  int ans = 0;
  for (int i = 0; i < N; ++i) { ans += total(i, cards, cache); }
  return ans;
}

int main() {
  vector<Card> cards;
  ifstream fin("input/day04.in");
  string s;
  while (getline(fin, s)) {
    Card card = Card();
    istringstream ss(s);
    ss >> s;
    ss >> s;
    s = s.substr(0, s.size() - 1);
    card.number = stoi(s);
    while (ss >> s && s.compare("|")) { card.winning.insert(stoi(s)); }
    while (ss >> s) { card.youHave.insert(stoi(s)); }
    cards.push_back(card);
  }
  fin.close();
  cout << "Part 1: " << part1(cards) << endl;
  cout << "Part 2: " << part2(cards) << endl;
  return 0;
}
