#include <bits/stdc++.h>

using namespace std;

struct Condition {
  string rating;
  bool greaterThan;
  int target;
};

struct Workflow {
  vector<tuple<Condition, string>> conditions;
  string fallback;
};

struct Part {
  unordered_map<string, int> ratings;
};

unordered_map<string, Workflow> workflows;
vector<Part> parts;

void input() {
  ifstream fin("input/day19.in");
  string s;
  while (getline(fin, s)) {
    if (!s.empty()) {
      if (s[0] == '{') {
        Part p;
        int N = s.size();
        string label = "";
        int rating = 0;
        bool inLabel = true;
        for (int i = 1; i < N; ++i) {
          if (s[i] == ',' || s[i] == '}') {
            p.ratings[label] = rating;
            label = "";
            rating = 0;
            inLabel = true;
          } else if (s[i] == '=') {
            inLabel = false;
          } else if (inLabel) {
            label += s[i];
          } else {
            rating = rating * 10 + (s[i] - '0');
          }
        }
        parts.push_back(p);
      } else {
        string label = "";
        string target = "";
        Workflow w;
        int N = s.size();
        bool inMainLabel = true;
        bool inCompLabel = true;
        bool inTarget = false;
        Condition c;
        c.rating = "";
        c.greaterThan = false;
        c.target = 0;
        for (int i = 0; i < N; ++i) {
          if (s[i] == '{') {
            inMainLabel = false;
          } else if (inMainLabel) {
            label += s[i];
          } else if (s[i] == '>') {
            c.greaterThan = true;
            inCompLabel = false;
          } else if (s[i] == '<') {
            c.greaterThan = false;
            inCompLabel = false;
          } else if (s[i] == ':') {
            inTarget = true;
          } else if (s[i] == ',') {
            w.conditions.push_back({c, target});
            inCompLabel = true;
            inTarget = false;
            target = "";
            c.rating = "";
            c.greaterThan = false;
            c.target = 0;
          } else if (s[i] == '}') {
            w.fallback = c.rating;
          } else if (inCompLabel) {
            c.rating += s[i];
          } else if (inTarget) {
            target += s[i];
          } else if (!inCompLabel) {
            c.target = c.target * 10 + (s[i] - '0');
          }
        }
        workflows[label] = w;
      }
    }
  }
  fin.close();
}

bool matches(Condition& c, Part& p) {
  if (c.greaterThan) { return p.ratings[c.rating] > c.target; }
  return p.ratings[c.rating] < c.target;
}

int part1() {
  int ans = 0;
  for (Part& p : parts) {
    string wf = "in";
    while (wf != "A" && wf != "R") {
      Workflow& w = workflows[wf];
      vector<tuple<Condition, string>>& cs = w.conditions;
      bool found = false;
      for (tuple<Condition, string>& c : cs) {
        if (matches(get<0>(c), p)) {
          wf = get<1>(c);
          found = true;
          break;
        }
      }
      if (!found) { wf = w.fallback; }
    }
    if (wf == "A") {
      for (auto itr = p.ratings.begin(); itr != p.ratings.end(); ++itr) {
        ans += itr->second;
      }
    }
  }
  return ans;
}

long long ways(int minX,
               int maxX,
               int minM,
               int maxM,
               int minA,
               int maxA,
               int minS,
               int maxS,
               string w) {
  long long ans = 0L;
  if (maxX < minX || maxM < minM || maxA < minA || maxS < minS) { return ans; }
  if (w == "R") { return ans; }
  if (w == "A") {
    return (maxX - minX + 1L) * (maxM - minM + 1L) * (maxA - minA + 1L) *
           (maxS - minS + 1L);
  }
  vector<tuple<Condition, string>>& cs = workflows[w].conditions;
  for (tuple<Condition, string>& ct : cs) {
    Condition& c = get<0>(ct);
    assert(c.rating == "x" || c.rating == "m" || c.rating == "a" ||
           c.rating == "s");
    string t = get<1>(ct);
    if (c.greaterThan) {
      if (c.rating == "x" && c.target + 1 <= maxX) {
        ans += ways(c.target + 1, maxX, minM, maxM, minA, maxA, minS, maxS, t);
      }
      if (c.rating == "m" && c.target + 1 <= maxM) {
        ans += ways(minX, maxX, c.target + 1, maxM, minA, maxA, minS, maxS, t);
      }
      if (c.rating == "a" && c.target + 1 <= maxA) {
        ans += ways(minX, maxX, minM, maxM, c.target + 1, maxA, minS, maxS, t);
      }
      if (c.rating == "s" && c.target + 1 <= maxS) {
        ans += ways(minX, maxX, minM, maxM, minA, maxA, c.target + 1, maxS, t);
      }
    } else {
      if (c.rating == "x" && c.target - 1 >= minX) {
        ans += ways(minX, c.target - 1, minM, maxM, minA, maxA, minS, maxS, t);
      }
      if (c.rating == "m" && c.target - 1 >= minM) {
        ans += ways(minX, maxX, minM, c.target - 1, minA, maxA, minS, maxS, t);
      }
      if (c.rating == "a" && c.target - 1 >= minA) {
        ans += ways(minX, maxX, minM, maxM, minA, c.target - 1, minS, maxS, t);
      }
      if (c.rating == "s" && c.target - 1 >= minS) {
        ans += ways(minX, maxX, minM, maxM, minA, maxA, minS, c.target - 1, t);
      }
    }
    if (c.greaterThan) {
      if (c.rating == "x") { maxX = c.target; }
      if (c.rating == "m") { maxM = c.target; }
      if (c.rating == "a") { maxA = c.target; }
      if (c.rating == "s") { maxS = c.target; }
    } else {
      if (c.rating == "x") { minX = c.target; }
      if (c.rating == "m") { minM = c.target; }
      if (c.rating == "a") { minA = c.target; }
      if (c.rating == "s") { minS = c.target; }
    }
  }
  if (maxX >= minX && maxM >= minM && maxA >= minA && maxS >= minS) {
    ans += ways(minX, maxX, minM, maxM, minA, maxA, minS, maxS,
                workflows[w].fallback);
  }
  return ans;
}

long long part2() { return ways(1, 4000, 1, 4000, 1, 4000, 1, 4000, "in"); }

int main() {
  input();
  int p1 = part1();
  cout << "Part 1: " << p1 << endl;
  long long p2 = part2();
  cout << "Part 2: " << p2 << endl;
  return 0;
}
