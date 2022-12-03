var fs = require('fs');
var input = fs.readFileSync('inputs/05.input', 'utf8');

var EPS = 1e-9;
var segments = [];

input.split('\n').forEach(line => {
    if (line == "") return;
    var lineSplit = line.split(' -> ');
    var fromSplit = lineSplit[0].split(',');
    var toSplit = lineSplit[1].split(',');
    segments.push({
        from: {
            x: parseInt(fromSplit[0]),
            y: parseInt(fromSplit[1])
        },
        to: {
            x: parseInt(toSplit[0]),
            y: parseInt(toSplit[1])
        }
    });
});

var isHorizontal = (seg) => seg.from.x == seg.to.x || seg.from.y == seg.to.y;

var intersectionPoints = (seg1, seg2) => {
    var lineFromPoints = (p, q) => {
        a = p.y - q.y;
        b = q.x - p.x;
        c = -a * p.x - b * p.y;
        z = Math.sqrt(a * a + b * b);
        if (Math.abs(z) > EPS) {
            a /= z;
            b /= z;
            c /= z;
        }
        return { a, b, c };
    }

    var intersect1d = (a, b, c, d) => {
        if (a > b) [a, b] = [b, a];
        if (c > d) [c, d] = [d, c];
        return Math.max(a, c) <= Math.min(b, d) + EPS;
    }

    var det = (a, b, c, d) => a * d - b * c;

    var between = (l, r, x) => Math.min(l, r) <= x + EPS && x <= Math.max(l, r) + EPS;

    var distTo = (line, point) => line.a * point.x + line.b * point.y + line.c;

    var before = (point1, point2) => point1.x < point2.x - EPS || (Math.abs(point1.x - point2.x) < EPS && point1.y < point2.y - EPS);

    if (!intersect1d(seg1.from.x, seg1.to.x, seg2.from.x, seg2.to.x) ||
        !intersect1d(seg1.from.y, seg1.to.y, seg2.from.y, seg2.to.y)) {
        return [];
    }

    var m = lineFromPoints(seg1.from, seg1.to);
    var n = lineFromPoints(seg2.from, seg2.to);
    var zn = det(m.a, m.b, n.a, n.b);
    if (Math.abs(zn) >= EPS) {
        x = -det(m.c, m.b, n.c, n.b) / zn;
        y = -det(m.a, m.c, n.a, n.c) / zn;
        if (between(seg1.from.x, seg1.to.x, x) && between(seg1.from.y, seg1.to.y, y) &&
            between(seg2.from.x, seg2.to.x, x) && between(seg2.from.y, seg2.to.y, y)) {
            // Avoid intersections at half squares.
            if (Math.round(x * 10) % 10 == 5 || Math.round(y * 10) % 10 == 5)
                return [];
            return [{ x: Math.round(x), y: Math.round(y) }];
        }
    } else {
        if (Math.abs(distTo(m, seg2.from)) > EPS || Math.abs(distTo(n, seg1.from)) > EPS)
            return [];
        if (before(seg1.to, seg1.from)) [seg1.to, seg1.from] = [seg1.from, seg1.to];
        if (before(seg2.to, seg2.from)) [seg2.to, seg2.from] = [seg2.from, seg2.to];
        var left = seg1.from;
        if (before(seg1.from, seg2.from)) left = seg2.from;
        var right = seg1.to;
        if (before(seg2.to, seg1.to)) right = seg2.to;
        var ans = [];
        ans.push(left);
        while (before(left, right)) {
            var next = {
                x: Math.round(left.x + Math.sign(right.x - left.x)),
                y: Math.round(left.y + Math.sign(right.y - left.y))
            };
            ans.push(next);
            left = next;
        }
        return ans;
    }
    return [];
}

var horizontalIntersections = new Set();
var intersections = new Set();

for (var i = 0; i < segments.length; ++i) {
    for (var j = i + 1; j < segments.length; ++j) {
        var points = intersectionPoints(segments[i], segments[j]);
        for (var k = 0; k < points.length; ++k) {
            if (isHorizontal(segments[i]) && isHorizontal(segments[j]))
                horizontalIntersections.add(points[k].x + "," + points[k].y);
            intersections.add(points[k].x + "," + points[k].y);
        }
    }
}

console.log("Part 1: " + horizontalIntersections.size);
console.log("Part 2: " + intersections.size);
