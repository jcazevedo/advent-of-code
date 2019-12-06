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

map<string, vector<string> > buildOrbitGraph(vector<pair<string, string> > orbits) {
  map<string, vector<string> > graph;
  for (pair<string, string> orbit : orbits)
    graph[orbit.second].push_back(orbit.first);
  return graph;
}

map<string, vector<string> > buildBidirectionalGraph(vector<pair<string, string> > orbits) {
  map<string, vector<string> > graph;
  for (pair<string, string> orbit : orbits) {
    graph[orbit.second].push_back(orbit.first);
    graph[orbit.first].push_back(orbit.second);
  }
  return graph;
}

int getDistance(const map<string, vector<string> >& graph, string start, string end) {
  string s = graph.at(start).front();
  string e = graph.at(end).front();
  map<string, int> dist;
  queue<string> q;
  q.push(s);
  dist[s] = 0;
  while (!q.empty()) {
    string curr = q.front(); q.pop();
    vector<string> nextPlanets = graph.at(curr);
    for (string next : nextPlanets) {
      if (dist.find(next) == dist.end()) {
        if (next == e)
          return dist[curr] + 1;
        dist[next] = dist[curr] + 1;
        q.push(next);
      }
    }
  }
  return dist[e];
}

int main() {
  vector<pair<string, string> > orbits = readInput();
  map<string, vector<string> > graph = buildOrbitGraph(orbits);
  cout << "Part 1: " << countOrbits(graph) << endl;
  map<string, vector<string> > completeGraph = buildBidirectionalGraph(orbits);
  cout << "Part 2: " << getDistance(completeGraph, "YOU", "SAN") << endl;
  return 0;
}
