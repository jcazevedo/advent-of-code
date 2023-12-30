#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

map<tuple<int, int>, vector<tuple<int, int>>> NEXT_DIRS = {
    {{0, 1}, {{0, 1}, {-1, 0}, {1, 0}}},
    {{0, -1}, {{0, -1}, {-1, 0}, {1, 0}}},
    {{-1, 0}, {{-1, 0}, {0, -1}, {0, 1}}},
    {{1, 0}, {{1, 0}, {0, -1}, {0, 1}}}};

vector<vector<int>> grid;
int H, W;

void input() {
  ifstream fin("input/day17.in");
  string tmp;
  while (fin >> tmp) {
    vector<int> heat;
    for (char ch : tmp) { heat.push_back(ch - '0'); }
    grid.push_back(heat);
  }
  fin.close();
  H = grid.size();
  W = grid[0].size();
}

struct Node {
  int i, j, consecutive, heat;
  tuple<int, int> dir;
};

int solve(int minConsecutive, int maxConsecutive) {
  map<tuple<int, int, tuple<int, int>, int>, int> dist;
  auto compare = [](const Node& a, const Node& b) { return a.heat > b.heat; };
  priority_queue<Node, vector<Node>, decltype(compare)> pq(compare);
  Node start = {0, 0, 0, 0, {0, 1}};
  pq.push(start);
  dist[{0, 0, {0, 1}, 0}] = 0;
  start.dir = {1, 0};
  pq.push(start);
  dist[{0, 0, {1, 0}, 0}] = 0;
  while (!pq.empty()) {
    Node curr = pq.top();
    pq.pop();
    if (curr.i == H - 1 && curr.j == W - 1 &&
        curr.consecutive >= minConsecutive) {
      return curr.heat;
    }
    for (const tuple<int, int>& nd : NEXT_DIRS[curr.dir]) {
      if (curr.consecutive < minConsecutive && nd != curr.dir) { continue; }
      if (curr.consecutive == maxConsecutive && nd == curr.dir) { continue; }
      int ni = curr.i + get<0>(nd);
      int nj = curr.j + get<1>(nd);
      int nextConsecutive = 1;
      if (nd == curr.dir) { nextConsecutive = curr.consecutive + 1; }
      if (ni >= 0 && ni < H && nj >= 0 && nj < W &&
          (dist.find({ni, nj, nd, nextConsecutive}) == dist.end() ||
           dist[{ni, nj, nd, nextConsecutive}] > curr.heat + grid[ni][nj])) {
        Node next;
        next.i = ni;
        next.j = nj;
        next.consecutive = nextConsecutive;
        next.heat = curr.heat + grid[ni][nj];
        next.dir = nd;
        dist[{ni, nj, nd, nextConsecutive}] = next.heat;
        pq.push(next);
      }
    }
  }
  return -1;
}

int part1() { return solve(0, 3); }

int part2() { return solve(4, 10); }

int main() {
  input();
  int p1 = part1();
  cout << "Part 1: " << p1 << endl;
  int p2 = part2();
  cout << "Part 2: " << p2 << endl;
  return 0;
}
