#include <fstream>
#include <iostream>
#include <queue>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

#define FLIP_FLOP 0
#define CONJUNCTION 1
#define BROADCASTER 2

#define HIGH_PULSE 0
#define LOW_PULSE 1

#define ON 0
#define OFF 1

unordered_map<string, vector<string>> connections;
unordered_map<string, int> type;
unordered_map<string, int> status;
unordered_map<string, unordered_map<string, int>> previousInput;

void input() {
  ifstream fin("input/day20.in");
  string line;
  while (getline(fin, line)) {
    istringstream ss(line);
    string tmp, current;
    ss >> current;
    if (current[0] == '%') {
      current = current.substr(1, current.size() - 1);
      type[current] = FLIP_FLOP;
      status[current] = OFF;
    } else if (current[0] == '&') {
      current = current.substr(1, current.size() - 1);
      type[current] = CONJUNCTION;
      status[current] = OFF;
    } else {
      type[current] = BROADCASTER;
      status[current] = OFF;
    }
    while (ss >> tmp) {
      if (tmp != "->") {
        if (tmp.back() == ',') { tmp = tmp.substr(0, tmp.size() - 1); }
        connections[current].push_back(tmp);
        previousInput[tmp][current] = LOW_PULSE;
      }
    }
  }
  fin.close();
}

struct Signal {
  string from, to;
  int pulse;

  Signal(string _from, string _to, int _pulse)
      : from(_from), to(_to), pulse(_pulse) {}
};

void reset() {
  for (auto itr = connections.begin(); itr != connections.end(); ++itr) {
    status[itr->first] = OFF;
    for (auto itr2 = previousInput[itr->first].begin();
         itr2 != previousInput[itr->first].end(); ++itr2) {
      previousInput[itr->first][itr2->first] = LOW_PULSE;
    }
  }
}

void press(function<void(Signal)> send,
           function<bool()> done,
           function<Signal()> getNext) {
  send(Signal("button", "broadcaster", LOW_PULSE));
  while (!done()) {
    Signal current = getNext();
    previousInput[current.to][current.from] = current.pulse;
    if (type[current.to] == FLIP_FLOP) {
      if (current.pulse == LOW_PULSE) {
        if (status[current.to] == OFF) {
          status[current.to] = ON;
          for (string& connection : connections[current.to]) {
            send(Signal(current.to, connection, HIGH_PULSE));
          }
        } else if (status[current.to] == ON) {
          status[current.to] = OFF;
          for (string& connection : connections[current.to]) {
            send(Signal(current.to, connection, LOW_PULSE));
          }
        }
      }
    }
    if (type[current.to] == CONJUNCTION) {
      bool allHigh = true;
      for (auto itr = previousInput[current.to].begin();
           itr != previousInput[current.to].end(); ++itr) {
        if (itr->second == LOW_PULSE) {
          allHigh = false;
          break;
        }
      }
      for (string& connection : connections[current.to]) {
        send(Signal(current.to, connection, allHigh ? LOW_PULSE : HIGH_PULSE));
      }
    }
    if (type[current.to] == BROADCASTER) {
      for (string& connection : connections[current.to]) {
        send(Signal(current.to, connection, current.pulse));
      }
    }
  }
}

long long part1() {
  reset();
  long long low = 0, high = 0;
  queue<Signal> q;
  for (int i = 0; i < 1000; ++i) {
    press(
        [&](Signal signal) {
          if (signal.pulse == LOW_PULSE) {
            ++low;
          } else {
            ++high;
          }
          q.push(signal);
        },
        [&]() { return q.empty(); },
        [&]() {
          Signal ans = q.front();
          q.pop();
          return ans;
        });
  }
  return low * high;
}

long long gcd(long long a, long long b) {
  if (b == 0) { return a; }
  return gcd(b, a % b);
}

long long lcm(long long a, long long b) { return (a / gcd(a, b)) * b; }

long long part2() {
  queue<Signal> q;
  // All these need to get a high pulse.
  vector<string> target = {"rk", "cd", "zf", "qx"};
  vector<long long> pressesToHigh = {0L, 0L, 0L, 0L};
  int N = target.size();
  for (int i = 0; i < N; ++i) {
    bool done = false;
    reset();
    while (!q.empty()) { q.pop(); }
    while (!done) {
      pressesToHigh[i]++;
      press(
          [&](Signal signal) {
            if (signal.from == target[i] && signal.pulse == HIGH_PULSE) {
              done = true;
            }
            q.push(signal);
          },
          [&]() { return done || q.empty(); },
          [&]() {
            Signal ans = q.front();
            q.pop();
            return ans;
          });
    }
  }
  long long ans = 1L;
  for (long long v : pressesToHigh) { ans = lcm(ans, v); }
  return ans;
}

int main() {
  input();
  long long p1 = part1();
  cout << "Part 1: " << p1 << endl;
  long long p2 = part2();
  cout << "Part 2: " << p2 << endl;
  return 0;
}
