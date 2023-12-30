#include <cassert>
#include <fstream>
#include <iostream>
#include <limits>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

unordered_map<string, set<string>> graph;

void input() {
  ifstream fin("input/day25.in");
  string line;
  while (getline(fin, line)) {
    istringstream ss(line);
    string u, v;
    ss >> u;
    u = u.substr(0, u.size() - 1);
    while (ss >> v) {
      graph[u].insert(v);
      graph[v].insert(u);
    }
  }
  fin.close();
}

int part1() {
  int N = 0;
  unordered_map<string, int> idx;
  for (auto itr = graph.begin(); itr != graph.end(); ++itr) {
    idx[itr->first] = N++;
  }
  vector<vector<int>> edge = vector<vector<int>>(N, vector<int>(N, 0));
  for (auto itr = graph.begin(); itr != graph.end(); ++itr) {
    string u = itr->first;
    for (string v : itr->second) {
      edge[idx[u]][idx[v]] = 1;
      edge[idx[v]][idx[u]] = 1;
    }
  }
  pair<int, vector<int>> best = {numeric_limits<int>::max(), {}};
  vector<vector<int>> co(N);
  for (int i = 0; i < N; ++i) { co[i] = {i}; }
  for (int ph = 1; ph < N; ++ph) {
    vector<int> w = edge[0];
    int s = 0, t = 0;
    for (int it = 0; it < N - ph; ++it) {
      w[t] = numeric_limits<int>::min();
      s = t;
      t = max_element(w.begin(), w.end()) - w.begin();
      for (int i = 0; i < N; ++i) { w[i] += edge[t][i]; }
    }
    best = min(best, {w[t] - edge[t][t], co[t]});
    co[s].insert(co[s].end(), co[t].begin(), co[t].end());
    for (int i = 0; i < N; ++i) { edge[s][i] += edge[t][i]; }
    for (int i = 0; i < N; ++i) { edge[i][s] = edge[s][i]; }
    edge[0][t] = numeric_limits<int>::min();
  }
  assert(best.first == 3);
  return (N - best.second.size()) * best.second.size();
}

int main() {
  input();
  int p1 = part1();
  cout << "Part 1: " << p1 << endl;
  return 0;
}
