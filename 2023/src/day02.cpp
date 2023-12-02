#include <algorithm>
#include <cassert>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

struct Reveal {
  int red;
  int green;
  int blue;

  Reveal(int _red, int _green, int _blue)
      : red(_red), green(_green), blue(_blue) {}
};

struct Game {
  int id;
  vector<Reveal> reveals;

  Game(int _id, vector<Reveal> _reveals) : id(_id), reveals(_reveals) {}
};

vector<Reveal> loadReveals(string s) {
  vector<Reveal> ans;
  Reveal curr = Reveal(0, 0, 0);
  istringstream ss(s);
  int quantity;
  string color;
  while (ss >> quantity >> color) {
    bool finish = false;
    if (color.back() == ';') { finish = true; }
    if (color.back() == ';' || color.back() == ',') {
      color = color.substr(0, color.size() - 1);
    }
    if (color.compare("red") == 0) {
      curr.red = quantity;
    } else if (color.compare("green") == 0) {
      curr.green = quantity;
    } else if (color.compare("blue") == 0) {
      curr.blue = quantity;
    } else {
      assert(false);
    }
    if (finish) {
      ans.push_back(curr);
      curr = Reveal(0, 0, 0);
    }
  }
  ans.push_back(curr);
  return ans;
}

Game loadGame(string s) {
  int id;
  string revealString(1000, '\0');
  sscanf(s.c_str(), "Game %d: %999[^\n]", &id,
         const_cast<char*>(revealString.c_str()));
  revealString.resize(revealString.find('\0'));
  return Game(id, loadReveals(revealString));
}

int part1(const vector<Game>& games) {
  int maxRed = 12, maxGreen = 13, maxBlue = 14, ans = 0;
  for (const Game& game : games) {
    bool good = true;
    for (const Reveal& reveal : game.reveals) {
      if (reveal.red > maxRed || reveal.green > maxGreen ||
          reveal.blue > maxBlue) {
        good = false;
        break;
      }
    }
    if (good) { ans += game.id; }
  }
  return ans;
}

int part2(const vector<Game>& games) {
  int ans = 0;
  for (const Game& game : games) {
    int maxRed = 0, maxGreen = 0, maxBlue = 0;
    for (const Reveal& reveal : game.reveals) {
      maxRed = max(maxRed, reveal.red);
      maxGreen = max(maxGreen, reveal.green);
      maxBlue = max(maxBlue, reveal.blue);
    }
    ans += maxRed * maxGreen * maxBlue;
  }
  return ans;
}

int main() {
  vector<Game> games;
  ifstream fin("input/day02.in");
  string s;
  while (getline(fin, s)) { games.push_back(loadGame(s)); }
  fin.close();
  cout << "Part 1: " << part1(games) << endl;
  cout << "Part 2: " << part2(games) << endl;
  return 0;
}
