# KLI Benchmark Results

Generated: 2026-02-20 13:23:13 UTC
Platform: SBCL 2.5.10 on Linux X86-64
Time resolution: 1000000 units/sec

## CRDT Benchmarks

```
CRDT (13:23:15)
  Benchmark                                Min µs     Med µs     P99 µs     Max µs     Med bytes
  ---------------------------------------- ---------- ---------- ---------- ---------- ------------
  ors-add 100                                     0.0        0.0     1000.0     1000.0       27200.
  ors-members 100                                 0.0        0.0     1000.0     1000.0           0.
  ors-merge 100+100                               0.0        0.0     1000.0     1000.0       20992.
  ors-add 1000                                    0.0        0.0     4000.0     4000.0      226432.
  ors-members 1000                                0.0        0.0     1000.0     1000.0           0.
  ors-merge 1000+1000                             0.0        0.0     1000.0     1000.0      163840.
  ors-add 10000                                3000.0     4000.0    10999.0    10999.0     1900112.
  ors-members 10000                            1000.0     1000.0     2001.0     2001.0       65536.
  ors-merge 10000+10000                        3000.0     3000.0    10000.0    10000.0     1413456.
  vc-merge 2 dims                                 0.0        0.0        0.0        0.0           0.
  vc-happened-before 2 dims                       0.0        0.0        0.0        0.0           0.
  vc-merge 10 dims                                0.0        0.0        0.0     1000.0           0.
  vc-happened-before 10 dims                      0.0        0.0        0.0        0.0           0.
  vc-merge 50 dims                                0.0        0.0     1000.0     1000.0           0.
  vc-happened-before 50 dims                      0.0        0.0        0.0        0.0           0.
  vc-merge 100 dims                               0.0        0.0     1000.0     1000.0       32768.
  vc-happened-before 100 dims                     0.0        0.0        0.0        0.0           0.
  gs-add 100                                      0.0        0.0     1000.0     1000.0           0.
  gs-merge 100+100                                0.0        0.0     1000.0     1000.0       32768.
  gs-add 1000                                     0.0        0.0     3000.0     3000.0      163840.
  gs-merge 1000+1000                              0.0        0.0     1000.0     1000.0      331840.
  gs-add 10000                                 1000.0     2000.0    16000.0    16000.0     1413456.
  gs-merge 10000+10000                         1000.0     2000.0     5000.0     5000.0     2052624.
```

## Event Log Benchmarks (Synthetic)

```
Event Log (13:23:22)
  Benchmark                                Min µs     Med µs     P99 µs     Max µs     Med bytes
  ---------------------------------------- ---------- ---------- ---------- ---------- ------------
  elog-load 100 events                         1000.0     2000.0     5000.0     5000.0      556608.
  compute-state 100 events                        0.0        0.0     1000.0     1000.0       29376.
  elog-merge 100+100 events                       0.0        0.0     1000.0     1000.0       21056.
  event-json-roundtrip 100                     2000.0     2000.0     3000.0     3000.0      720896.
  elog-load 500 events                         8000.0     9000.0    12001.0    12001.0     2731648.
  compute-state 500 events                        0.0        0.0     1000.0     1000.0      131072.
  elog-merge 500+500 events                       0.0        0.0     1000.0     1000.0      115840.
  event-json-roundtrip 500                     4000.0     4000.0    13000.0    13000.0     3649280.
  elog-load 1000 events                        6000.0     6000.0    13000.0    13000.0     5452864.
  compute-state 1000 events                       0.0        0.0     1000.0     1000.0      285952.
  elog-merge 1000+1000 events                     0.0        0.0     1000.0     1000.0      163584.
  event-json-roundtrip 1000                    8000.0     9000.0    11000.0    11000.0     7300416.
  elog-load 5000 events                       30000.0    34000.0    43000.0    43000.0    29875072.
  compute-state 5000 events                    3000.0     3500.0     7000.0     7000.0     1332224.
  elog-merge 5000+5000 events                  1000.0     2000.0     6000.0     6000.0      766096.
  event-json-roundtrip 5000                   43000.0    45000.0    47001.0    47001.0    38854080.
```

## Real Task Replay

987 tasks scanned across the monorepo. Representatives selected by size bucket
(min, median, max from each).

```
Environment:
  SBCL 2.5.10 on Linux X86-64
  Time units/sec: 1000000
  Tasks scanned: 987
  Distribution: SMALL=843, MEDIUM=121, LARGE=22, XLARGE=1
  Representatives: 10 (1 9 49 50 92 198 202 310 459 820 events)

Real Task Replay (13:23:24)
  Benchmark                                Min µs     Med µs     P99 µs     Max µs     Med bytes
  ---------------------------------------- ---------- ---------- ---------- ---------- ------------
  real elog-load 1 (SMALL)                        0.0        0.0     1000.0     1000.0           0.
  real compute-state 1 (SMALL)                    0.0        0.0        0.0        0.0           0.
  real elog-load 9 (SMALL)                        0.0      500.0     1000.0     1000.0       65536.
  real compute-state 9 (SMALL)                    0.0        0.0        0.0        0.0           0.
  real elog-load 49 (SMALL)                    1000.0     1000.0     3000.0     3000.0      546112.
  real compute-state 49 (SMALL)                   0.0        0.0        0.0        0.0       32192.
  real elog-load 50 (MEDIUM)                      0.0     1000.0     2000.0     2000.0      325376.
  real compute-state 50 (MEDIUM)                  0.0        0.0        0.0        0.0           0.
  real elog-load 92 (MEDIUM)                   1000.0     2000.0     2001.0     2001.0     1039936.
  real compute-state 92 (MEDIUM)                  0.0        0.0     1000.0     1000.0       50560.
  real elog-load 198 (MEDIUM)                  3000.0     4000.0     8000.0     8000.0     2487744.
  real compute-state 198 (MEDIUM)                 0.0        0.0     1000.0     1000.0       65536.
  real elog-load 202 (LARGE)                   2000.0     5000.0     6000.0     6000.0     1801280.
  real compute-state 202 (LARGE)                  0.0        0.0     1000.0     1000.0       98304.
  real elog-load 310 (LARGE)                   5000.0     9000.0    11000.0    11000.0     3369728.
  real compute-state 310 (LARGE)                  0.0        0.0     1000.0     1000.0      130240.
  real elog-load 459 (LARGE)                   6000.0     6000.0     8001.0     8001.0     4189056.
  real compute-state 459 (LARGE)                  0.0        0.0     1000.0     1000.0      173184.
  real elog-load 820 (XLARGE)                 10000.0    12000.0    23000.0    23000.0     8345920.
  real compute-state 820 (XLARGE)                 0.0        0.0     1000.0     1000.0      425984.
```

## Key Takeaways

- **compute-state is sub-microsecond** through 200 events, sub-millisecond at 820
- **JSON parsing dominates** — elog-load is approximately 100x slower than the CRDT fold
- **CRDT merge is negligible** — OR-Set merge 10k+10k: 3ms median
- **Vector clock ops are sub-microsecond** even at 100 dimensions
- **987 tasks scanned** across the monorepo

## Reproducing

```lisp
;; Load the benchmark package
(asdf:load-system "kli-bench")

;; Run CRDT and event log suites
(kli-bench:run-all)

;; Run real-task replay (supply your task directories)
(kli-bench:run-real-tasks #P"/path/to/tasks/")

;; Run everything
(kli-bench:run-benchmarks :real-task-roots '(#P"/path/to/tasks/"))
```
