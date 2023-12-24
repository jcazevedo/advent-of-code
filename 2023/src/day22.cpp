#include <bits/stdc++.h>

using namespace std;

struct Brick {
  tuple<int, int, int> from, to;
};

vector<Brick> bricks;
vector<bool> consider;
map<tuple<int, int, int>, int> filled;
int N;

void input() {
  ifstream fin("input/day22.in");
  string tmp;
  while (fin >> tmp) {
    Brick b;
    sscanf(tmp.c_str(), "%d,%d,%d~%d,%d,%d", &get<0>(b.from), &get<1>(b.from),
           &get<2>(b.from), &get<0>(b.to), &get<1>(b.to), &get<2>(b.to));
    bricks.push_back(b);
    consider.push_back(true);
  }
  fin.close();
  N = bricks.size();
}

int drop() {
  for (int i = 0; i < N; ++i) {
    if (!consider[i]) { continue; }
    for (int x = get<0>(bricks[i].from); x <= get<0>(bricks[i].to); ++x) {
      for (int y = get<1>(bricks[i].from); y <= get<1>(bricks[i].to); ++y) {
        for (int z = get<2>(bricks[i].from); z <= get<2>(bricks[i].to); ++z) {
          filled[{x, y, z}] = i + 1;
        }
      }
    }
  }
  set<int> dropped;
  while (true) {
    bool didDrop = false;
    for (int i = 0; i < N; ++i) {
      if (!consider[i]) { continue; }
      int z = get<2>(bricks[i].from);
      if (z == 1) { continue; }
      bool canDrop = true;
      for (int x = get<0>(bricks[i].from); x <= get<0>(bricks[i].to) && canDrop;
           ++x) {
        for (int y = get<1>(bricks[i].from);
             y <= get<1>(bricks[i].to) && canDrop; ++y) {
          if (filled[{x, y, z - 1}]) { canDrop = false; }
        }
      }
      if (canDrop) {
        didDrop = true;
        for (int x = get<0>(bricks[i].from); x <= get<0>(bricks[i].to); ++x) {
          for (int y = get<1>(bricks[i].from); y <= get<1>(bricks[i].to); ++y) {
            for (int z = get<2>(bricks[i].from); z <= get<2>(bricks[i].to);
                 ++z) {
              filled[{x, y, z}] = 0;
              filled[{x, y, z - 1}] = i + 1;
            }
          }
        }
        --get<2>(bricks[i].from);
        --get<2>(bricks[i].to);
        dropped.insert(i);
      }
    }
    if (!didDrop) { break; }
  }
  return dropped.size();
}

int part1() {
  auto prevBricks = bricks;
  auto prevFilled = filled;
  drop();
  int ans = 0;
  for (int i = 0; i < N; ++i) {
    set<int> supported;
    for (int x = get<0>(bricks[i].from); x <= get<0>(bricks[i].to); ++x) {
      for (int y = get<1>(bricks[i].from); y <= get<1>(bricks[i].to); ++y) {
        int f = filled[{x, y, get<2>(bricks[i].to) + 1}];
        if (f) { supported.insert(f); }
      }
    }
    if (supported.empty()) {
      ++ans;
    } else {
      bool good = true;
      for (int s : supported) {
        set<int> supporting;
        for (int x = get<0>(bricks[s - 1].from); x <= get<0>(bricks[s - 1].to);
             ++x) {
          for (int y = get<1>(bricks[s - 1].from);
               y <= get<1>(bricks[s - 1].to); ++y) {
            int f = filled[{x, y, get<2>(bricks[s - 1].from) - 1}];
            if (f) { supporting.insert(f); }
          }
        }
        if (supporting.size() == 1) {
          good = false;
          break;
        }
      }
      if (good) { ++ans; }
    }
  }
  bricks = prevBricks;
  filled = prevFilled;
  return ans;
}

int part2() {
  auto prevBricks = bricks;
  auto prevFilled = filled;
  drop();
  int ans = 0;
  for (int i = 0; i < N; ++i) {
    auto bb = bricks;
    auto ff = filled;
    for (int x = get<0>(bricks[i].from); x <= get<0>(bricks[i].to); ++x) {
      for (int y = get<1>(bricks[i].from); y <= get<1>(bricks[i].to); ++y) {
        for (int z = get<2>(bricks[i].from); z <= get<2>(bricks[i].to); ++z) {
          filled[{x, y, z}] = 0;
        }
      }
    }
    consider[i] = false;
    ans += drop();
    consider[i] = true;
    bricks = bb;
    filled = ff;
  }
  bricks = prevBricks;
  filled = prevFilled;
  return ans;
}

int main() {
  input();
  int p1 = part1();
  cout << "Part 1: " << p1 << endl;
  int p2 = part2();
  cout << "Part 2: " << p2 << endl;
  return 0;
}
