#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

string ii;
unordered_map<string, string> ll, rr;

void input() {
  ifstream fin("input/day08.in");
  fin >> ii;
  string sn, sl, sr, stmp;
  while (fin >> sn >> stmp >> sl >> sr) {
    ll[sn] = sl.substr(1, sl.find(",") - 1);
    rr[sn] = sr.substr(0, sr.find(")"));
  }
  fin.close();
}

int part1() {
  int ans = 0, i = 0;
  string curr = "AAA";
  while (curr != "ZZZ") {
    if (ii[i] == 'L') {
      curr = ll[curr];
    } else {
      curr = rr[curr];
    }
    i = (i + 1) % ii.size();
    ++ans;
  }
  return ans;
}

long long gcd(long long a, long long b) {
  if (b == 0) { return a; }
  return gcd(b, a % b);
}

long long lcm(long long a, long long b) { return (a / gcd(a, b)) * b; }

long long part2() {
  vector<int> length, repeat;
  vector<vector<int>> finals;
  for (unordered_map<string, string>::iterator itr = ll.begin();
       itr != ll.end(); ++itr) {
    if (itr->first.back() == 'A') {
      string curr = itr->first;
      int ans = 0, i = 0;
      map<tuple<string, int>, int> visited;
      vector<int> ff;
      while (!visited[{curr, i}]) {
        visited[{curr, i}] = ans;
        if (curr.back() == 'Z') { ff.push_back(ans); }
        if (ii[i] == 'L') {
          curr = ll[curr];
        } else {
          curr = rr[curr];
        }
        i = (i + 1) % ii.size();
        ++ans;
      }
      repeat.push_back(visited[{curr, i}]);
      finals.push_back(ff);
      length.push_back(ans);
    }
  }
  int N = length.size();
  // Took me a while to figure this out, but the input is well behaved to allow
  // us to use the LCM.
  for (int i = 0; i < N; ++i) {
    assert(finals[i].size() == 1);
    assert(finals[i][0] == (length[i] - repeat[i]));
  }
  long long ans = 1;
  for (int i = 0; i < N; ++i) { ans = lcm(ans, finals[i][0]); }
  return ans;
}

int main() {
  input();
  cout << "Part 1: " << part1() << endl;
  cout << "Part 2: " << part2() << endl;
  return 0;
}
