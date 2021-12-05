#include <bits/stdc++.h>

using namespace std;

#define W 25
#define H 6

vector<vector<vector<int> > > readInput() {
  ifstream fin("day08.in");
  string str; fin >> str;
  fin.close();
  vector<vector<vector<int> > > res;
  vector<vector<int> > currLayer;
  vector<int> currLine;
  int N = str.size(), i = 0, j = 0;
  for (int s = 0; s < N; ++s) {
    currLine.push_back(str[s] - '0');
    i++;
    if (i >= W) {
      currLayer.push_back(currLine);
      currLine.clear();
      j++;
      i = 0;
    }
    if (j >= H) {
      res.push_back(currLayer);
      currLayer.clear();
      j = 0;
    }
  }
  return res;
}

int part1(const vector<vector<vector<int> > > layers) {
  int best = numeric_limits<int>::max(), bestI = -1;
  int L = layers.size();
  for (int i = 0; i < L; ++i) {
    int cnt = 0;
    for (int j = 0; j < H; ++j) {
      for (int k = 0; k < W; ++k) {
        if (layers[i][j][k] == 0)
          cnt++;
      }
    }
    if (cnt < best) {
      best = cnt;
      bestI = i;
    }
  }
  int n1 = 0, n2 = 0;
  for (int i = 0; i < H; ++i) {
    for (int j = 0; j < W; ++j) {
      if (layers[bestI][i][j] == 1)
        n1++;
      if (layers[bestI][i][j] == 2)
        n2++;
    }
  }
  return n1 * n2;
}

void showImage(const vector<vector<vector<int> > > layers) {
  int L = layers.size();
  for (int i = 0; i < H; ++i) {
    for (int j = 0; j < W; ++j) {
      int k = 0;
      while (k < L && layers[k][i][j] == 2)
        k++;
      assert(k < L);
      cout << (layers[k][i][j] == 0 ? ' ' : '*');
    }
    cout << endl;
  }
}

int main() {
  vector<vector<vector<int> > > layers = readInput();
  cout << "Part 1: " << part1(layers) << endl;
  cout << "Part 2:" << endl;
  showImage(layers);
  return 0;
}
