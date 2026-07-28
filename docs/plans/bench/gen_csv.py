#!/usr/bin/env python3
"""Generate a representative large CSV for the 0026 measurements.

    python3 gen_csv.py /tmp/big.csv 33554432          # LF
    python3 gen_csv.py /tmp/big-crlf.csv 33554432 $'\\r\\n'

Generated rather than found, and deterministic (seeded), so the before/after
numbers in docs/plans/completed/0026 can be reproduced exactly. ~32 MB, 12
columns. Field mix per row:
  0 id (plain int)          6 quoted field containing the delimiter
  1 name (plain)            7 quoted field containing doubled quotes
  2 email (plain)           8 plain float
  3 city (plain)            9 ISO date
  4 plain word             10 every 500th row: a MULTI-LINE quoted cell
  5 quoted plain field     11 trailing plain field (some rows ragged: dropped)
"""
import random, sys

path = sys.argv[1]
target = int(sys.argv[2]) if len(sys.argv) > 2 else 32 * 1024 * 1024
eol = sys.argv[3] if len(sys.argv) > 3 else "\n"

rnd = random.Random(20260727)
words = ["alpha","bravo","charlie","delta","echo","foxtrot","golf","hotel",
         "india","juliet","kilo","lima","mike","november","oscar","papa"]
cities = ["Wellington","Auckland","Christchurch","Dunedin","Hamilton","Napier"]

out = []
size = 0
i = 0
out.append(eol.join([
  "id,name,email,city,word,quoted,with_comma,with_quote,amount,date,notes,tail"]) + eol)
size += len(out[0])
while size < target:
    i += 1
    w = rnd.choice(words); c = rnd.choice(cities)
    f = [
      str(i),
      "%s %s" % (w, rnd.choice(words)),
      "%s%d@example.com" % (w, i),
      c,
      rnd.choice(words),
      '"%s %s"' % (w, c),
      '"%s, %s"' % (c, w),
      '"he said ""%s"" loudly"' % w,
      "%.2f" % (rnd.random() * 10000),
      "20%02d-%02d-%02d" % (rnd.randrange(0, 30), rnd.randrange(1, 13), rnd.randrange(1, 29)),
      ('"line one%sline two%sline three"' % (eol, eol)) if i % 500 == 0 else rnd.choice(words),
      str(rnd.randrange(0, 1000)),
    ]
    if i % 997 == 0:          # ragged: short row
        f = f[:8]
    row = ",".join(f) + eol
    out.append(row)
    size += len(row)

with open(path, "w", newline="") as fh:
    fh.write("".join(out))
print("%s: %d bytes, %d rows" % (path, size, i + 1))
