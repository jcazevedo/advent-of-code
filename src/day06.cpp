#include <bits/stdc++.h>

using namespace std;

vector<pair<string, string> > readInput() {
  ifstream fin("day06.in");
  string str;
  vector<pair<string, string> > res;
  while (fin >> str) {
    replace(str.begin(), str.end(), ')', ' ');
    istringstream ss(str);
    pair<string, string> curr = make_pair("", "");
    ss >> curr.first >> curr.second;
    res.push_back(curr);
  }
  fin.close();
  return res;
}

int countPlanetOrbits(string planet, const map<string, vector<string> >& graph, map<string, int>& cache) {
  if (cache.find(planet) == cache.end()) {
    if (graph.find(planet) == graph.end()) {
      cache[planet] = 0;
    } else {
      int res = 0;
      vector<string> orbits = graph.at(planet);
      res += orbits.size();
      for (string planet : orbits) {
        res += countPlanetOrbits(planet, graph, cache);
      }
      cache[planet] = res;
    }
  }
  return cache[planet];
}

int countOrbits(const map<string, vector<string> >& graph) {
  map<string, int> cache;
  int tot = 0;
  for (auto itr = graph.begin(); itr != graph.end(); ++itr)
    tot += countPlanetOrbits(itr->first, graph, cache);
  return tot;
}

int main() {
  vector<pair<string, string> > orbits = readInput();
  map<string, vector<string> > graph;
  for (pair<string, string> orbit : orbits)
    graph[orbit.second].push_back(orbit.first);
  cout << "Part 1: " << countOrbits(graph) << endl;
  return 0;
}
