#include <bits/stdc++.h>

using namespace std;

#define EPS 10000
#define PI 3.14159265

int gcd(int a, int b) {
  if (b == 0)
    return a;
  return gcd(b, a % b);
}

double dist(double x1, double y1, double x2, double y2) {
  return sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
}

pair<pair<int, int>, int> getMaxAsteroidsSeen(const vector<string>& grid) {
  int best = 0, H = grid.size(), W = grid[0].size();
  pair<int, int> bestLoc = make_pair(-1, -1);
  for (int x = 0; x < W; ++x) {
    for (int y = 0; y < H; ++y) {
      if (grid[y][x] == '#') {
        set<pair<int, int> > vis;
        for (int nx = 0; nx < W; ++nx) {
          for (int ny = 0; ny < H; ++ny) {
            if ((nx == x && ny == y) || grid[ny][nx] == '.')
              continue;
            int dx = nx - x;
            int dy = ny - y;
            int gg = gcd(abs(dx), abs(dy));
            vis.insert({dx / gg, dy / gg});
          }
        }
        if (vis.size() > best) {
          best = vis.size();
          bestLoc = {x, y};
        }
      }
    }
  }
  return {bestLoc, best};
}

int getVaporized(pair<int, int> base, const vector<string>& grid) {
  int H = grid.size(), W = grid[0].size(), bx = base.first, by = base.second;
  map<int, set<tuple<double, int, int>>> points;
  for (int x = 0; x < W; ++x) {
    for (int y = 0; y < H; ++y) {
      if ((x == bx && y == by) || grid[y][x] == '.')
        continue;
      double dx = x - bx;
      double dy = y - by;
      double ang = acos(-dy / dist(dx, dy, 0, 0));
      if (dx < 0)
        ang = PI + (PI - ang);
      int angI = ang * EPS;
      points[angI].insert({dist(x, y, bx, by), x, y});
    }
  }
  int cnt = 0;
  while (true) {
    for (auto itr = points.begin(); itr != points.end(); ++itr) {
      auto& pp = itr->second;
      if (pp.size() > 0) {
        auto next = *pp.begin();
        cnt++;
        if (cnt == 200)
          return get<1>(next) * 100 + get<2>(next);
        pp.erase(pp.begin());
      }
    }
  }
}

int main() {
  ifstream fin("day10.in");
  vector<string> grid;
  string str;
  while (fin >> str)
    grid.push_back(str);
  fin.close();
  pair<pair<int, int>, int> locAndSize = getMaxAsteroidsSeen(grid);
  cout << "Part 1: " << locAndSize.second << endl;
  cout << "Part 2: " << getVaporized(locAndSize.first, grid) << endl;
  return 0;
}
