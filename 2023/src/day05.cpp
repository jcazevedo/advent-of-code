#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <sstream>
#include <vector>

using namespace std;

struct Range {
  long long destination;
  long long source;
  long long length;
};

vector<long long> seeds;
map<string, string> transitions;
map<string, vector<Range>> ranges;

long long lowestValue(string from,
                      string to,
                      long long source,
                      long long length) {
  if (from == to) { return source; }
  long long ans = numeric_limits<long long>::max();
  int N = ranges[from].size();
  int l = 0, r = N;
  while (l < r) {
    int m = l + (r - l) / 2;
    if (source >= ranges[from][m].source) {
      l = m + 1;
    } else {
      r = m;
    }
  }
  while (length != 0) {
    long long rangeLength, nextSource;
    if (l == 0 || (l < N && source >= ranges[from][l - 1].source +
                                          ranges[from][l - 1].length)) {
      nextSource = source;
      rangeLength = min(ranges[from][l].source - source, length);
      if (nextSource >= ranges[from][l].source) { ++l; }
    } else if (source <
               ranges[from][l - 1].source + ranges[from][l - 1].length) {
      nextSource =
          ranges[from][l - 1].destination + source - ranges[from][l - 1].source;
      rangeLength = min(
          ranges[from][l - 1].length - (source - ranges[from][l - 1].source),
          length);
      ++l;
    } else {
      nextSource = source;
      rangeLength = length;
    }
    ans = min(ans, lowestValue(transitions[from], to, nextSource, rangeLength));
    length -= rangeLength;
    source += rangeLength;
  }
  return ans;
}

long long part1() {
  long long ans = numeric_limits<long long>::max();
  for (long long seed : seeds) {
    ans = min(ans, lowestValue("seed", "location", seed, 1));
  }
  return ans;
}

long long part2() {
  int N = seeds.size();
  long long ans = numeric_limits<long long>::max();
  for (int i = 0; i < N; i += 2) {
    ans = min(ans, lowestValue("seed", "location", seeds[i], seeds[i + 1]));
  }
  return ans;
}

int main() {
  ifstream fin("input/day05.in");
  string s, tmp;
  getline(fin, s);
  istringstream ss(s);
  ss >> tmp;
  long long seed;
  while (ss >> seed) { seeds.push_back(seed); }
  getline(fin, s);
  string curr;
  while (getline(fin, s)) {
    if (s.size() == 0) { continue; }
    if (isdigit(s[0])) {
      Range r;
      istringstream ss(s);
      ss >> r.destination >> r.source >> r.length;
      ranges[curr].push_back(r);
    } else {
      size_t toLocation = s.find("-to-");
      string from = s.substr(0, toLocation);
      string to = s.substr(toLocation + 4, s.find(" ") - toLocation - 4);
      transitions[from] = to;
      curr = from;
    }
  }
  fin.close();
  for (map<string, vector<Range>>::iterator itr = ranges.begin();
       itr != ranges.end(); ++itr) {
    sort(itr->second.begin(), itr->second.end(),
         [&](Range a, Range b) { return a.source < b.source; });
  }
  cout << "Part 1: " << part1() << endl;
  cout << "Part 2: " << part2() << endl;
  return 0;
}
