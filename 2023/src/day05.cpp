#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <sstream>
#include <tuple>
#include <vector>

using namespace std;

struct Range {
  long long destination;
  long long source;
  long long length;
};

vector<long long> seeds;
map<tuple<string, string>, vector<Range>> ranges;

long long lowestValue(string from,
                      string to,
                      long long source,
                      long long length) {
  if (from == to) { return source; }
  long long ans = numeric_limits<long long>::max();
  for (auto itr = ranges.begin(); itr != ranges.end(); ++itr) {
    if (get<0>(itr->first) == from) {
      vector<Range>& currentRanges = itr->second;
      int N = currentRanges.size();
      while (length != 0) {
        int l = 0, r = N;
        while (l < r) {
          int m = l + (r - l) / 2;
          if (source >= currentRanges[m].source) {
            l = m + 1;
          } else {
            r = m;
          }
        }
        if (l == 0 || (l < N && source >= currentRanges[l - 1].source +
                                              currentRanges[l - 1].length)) {
          long long rangeLength = min(currentRanges[l].source - source, length);
          ans = min(ans,
                    lowestValue(get<1>(itr->first), to, source, rangeLength));
          source += rangeLength;
          length -= rangeLength;
        } else if (source <
                   currentRanges[l - 1].source + currentRanges[l - 1].length) {
          long long rangeLength =
              min(currentRanges[l - 1].length -
                      (source - currentRanges[l - 1].source),
                  length);
          ans = min(ans, lowestValue(get<1>(itr->first), to,
                                     currentRanges[l - 1].destination + source -
                                         currentRanges[l - 1].source,
                                     rangeLength));
          length -= rangeLength;
          source += rangeLength;
        } else {
          ans = min(ans, lowestValue(get<1>(itr->first), to, source, length));
          length = 0;
        }
      }
      break;
    }
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
  tuple<string, string> curr;
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
      curr = {from, to};
    }
  }
  fin.close();
  for (auto itr = ranges.begin(); itr != ranges.end(); ++itr) {
    sort(itr->second.begin(), itr->second.end(),
         [&](Range a, Range b) { return a.source < b.source; });
  }
  cout << "Part 1: " << part1() << endl;
  cout << "Part 2: " << part2() << endl;
  return 0;
}