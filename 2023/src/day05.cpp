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
      while (length != 0) {
        bool used = false;
        for (const Range& range : itr->second) {
          if (length == 0) { break; }
          if (source >= range.source && source < range.source + range.length) {
            long long rangeLength =
                min(range.length - (source - range.source), length);
            ans =
                min(ans, lowestValue(get<1>(itr->first), to,
                                     range.destination + source - range.source,
                                     rangeLength));
            length -= rangeLength;
            source += rangeLength;
            used = true;
            break;
          }
        }
        if (!used && length > 0) {
          long long minSource = numeric_limits<long long>::max();
          long long maxSource = numeric_limits<long long>::min();
          for (const Range& range : itr->second) {
            if (range.source > source) {
              minSource = min(minSource, range.source);
            }
            maxSource = max(maxSource, range.source + range.length);
          }
          if (source < minSource) {
            long long rangeLength = min(minSource - source, length);
            ans = min(ans,
                      lowestValue(get<1>(itr->first), to, source, rangeLength));
            source += rangeLength;
            length -= rangeLength;
          } else if (source >= maxSource) {
            ans = min(ans, lowestValue(get<1>(itr->first), to, source, length));
            length = 0;
          }
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
  cout << "Part 1: " << part1() << endl;
  cout << "Part 2: " << part2() << endl;
  return 0;
}
