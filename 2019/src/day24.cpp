#include <bits/stdc++.h>

using namespace std;

#define SIDE 5

bool bug(int state, int i, int j) {
  if (i < 0 || i >= SIDE || j < 0 || j >= SIDE)
    return false;
  int idx = i * SIDE + j;
  return (state & (1 << idx)) > 0;
}

int adjacent(int state, int i, int j) {
  static int dirs[][2] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};
  int cnt = 0;
  for (int d = 0; d < 4; ++d) {
    int ni = i + dirs[d][0];
    int nj = j + dirs[d][1];
    cnt += bug(state, ni, nj);
  }
  return cnt;
}

int update(int state, int i, int j, int v) {
  int idx = i * SIDE + j;
  if (v == 0)
    return state & ~(1 << idx);
  return state | (1 << idx);
}

int tick(int state) {
  static int cnts[SIDE][SIDE];
  memset(cnts, 0, sizeof(cnts));
  for (int i = 0; i < SIDE; ++i) {
    for (int j = 0; j < SIDE; ++j) {
      cnts[i][j] = adjacent(state, i, j);
    }
  }
  int res = state;
  for (int i = 0; i < SIDE; ++i) {
    for (int j = 0; j < SIDE; ++j) {
      if (bug(state, i, j) && cnts[i][j] != 1)
        res = update(res, i, j, 0);
      else if (!bug(state, i, j) && cnts[i][j] > 0 && cnts[i][j] < 3)
        res = update(res, i, j, 1);
    }
  }
  return res;
}

int initial(string file) {
  ifstream fin(file);
  string str;
  int res = 0;
  for (int i = 0; i < SIDE; ++i) {
    fin >> str;
    for (int j = 0; j < SIDE; ++j) {
      if (str[j] == '#')
        res = update(res, i, j, 1);
    }
  }
  return res;
}

int getFirstRepeat(int initial) {
  set<int> visited;
  int state = initial;
  visited.insert(state);
  while (true) {
    state = tick(state);
    if (visited.find(state) != visited.end())
      return state;
    visited.insert(state);
  }
}

int getBugsAux(map<int, int>& state, int nMinutes) {
  if (nMinutes == 0) {
    int cnt = 0;
    for (auto itr = state.begin(); itr != state.end(); ++itr) {
      int curr = itr->second;
      for (int i = 0; i < SIDE; ++i) {
        for (int j = 0; j < SIDE; ++j) {
          if (bug(curr, i, j))
            cnt++;
        }
      }
    }
    return cnt;
  } else {
    static int cnts[SIDE][SIDE];
    int maxL = numeric_limits<int>::min(), minL = numeric_limits<int>::max();
    for (auto itr = state.begin(); itr != state.end(); ++itr) {
      maxL = max(maxL, itr->first);
      minL = min(minL, itr->first);
    }
    maxL++;
    minL--;
    state[maxL] = 0;
    state[minL] = 0;
    int outU = 0, outD = 0, outL = 0, outR = 0;
    for (int l = maxL; l >= minL; --l) {
      int inU = l - 1 >= minL ? bug(state[l - 1], 1, 2) : 0;
      int inD = l - 1 >= minL ? bug(state[l - 1], 3, 2) : 0;
      int inL = l - 1 >= minL ? bug(state[l - 1], 2, 1) : 0;
      int inR = l - 1 >= minL ? bug(state[l - 1], 2, 3) : 0;
      int nextOutU = 0, nextOutD = 0, nextOutL = 0, nextOutR = 0;
      memset(cnts, 0, sizeof(cnts));
      for (int i = 0; i < SIDE; ++i) {
        for (int j = 0; j < SIDE; ++j) {
          if (i == SIDE / 2 && j == SIDE / 2)
            continue;
          int b = bug(state[l], i, j);
          if (i == 0)
            nextOutU += b;
          if (i == SIDE - 1)
            nextOutD += b;
          if (j == 0)
            nextOutL += b;
          if (j == SIDE - 1)
            nextOutR += b;
          cnts[i][j] = adjacent(state[l], i, j);
          if (i == 0)
            cnts[i][j] += inU;
          if (i == SIDE - 1)
            cnts[i][j] += inD;
          if (j == 0)
            cnts[i][j] += inL;
          if (j == SIDE - 1)
            cnts[i][j] += inR;
        }
      }
      cnts[1][2] += outU;
      cnts[3][2] += outD;
      cnts[2][1] += outL;
      cnts[2][3] += outR;
      int res = state[l];
      for (int i = 0; i < SIDE; ++i) {
        for (int j = 0; j < SIDE; ++j) {
          if (bug(res, i, j) && cnts[i][j] != 1)
            res = update(res, i, j, 0);
          else if (!bug(res, i, j) && cnts[i][j] > 0 && cnts[i][j] < 3)
            res = update(res, i, j, 1);
        }
      }
      state[l] = res;
      outU = nextOutU;
      outD = nextOutD;
      outL = nextOutL;
      outR = nextOutR;
    }
    return getBugsAux(state, nMinutes - 1);
  }
}

int getBugs(int state, int nMinutes) {
  map<int, int> initial = {{0, state}};
  return getBugsAux(initial, nMinutes);
}

int main() {
  int state = initial("day24.in");
  cout << "Part 1: " << getFirstRepeat(state) << endl;
  cout << "Part 2: " << getBugs(state, 200) << endl;
  return 0;
}
