# Graphs (alga only at the moment)

Benchmarks for graph data structures: AdjacencyMaps, Relation, Algebra.Graph, ...

## Running

For all benchmarks:

    $ stack bench :space :time

For just space:

    $ stack bench :space

For just time:

    $ stack bench :time

## Clique

| Case                               | Total bytes | Max residency | Final live | GCs |
|------------------------------------|-------------|---------------|------------|-----|
| clique [1..10] as Algebra.Graph    | 1,656       | 0             | 0          |   0 |
| clique [1..10] as AdjacencyMap     | 48,472      | 31,864        | 31,864     |   0 |
| clique [1..10] as IntAdjacencyMap  | 44,944      | 31,864        | 31,864     |   0 |
| clique [1..10] as Relation         | 59,464      | 32,256        | 32,256     |   0 |
| clique [1..100] as Algebra.Graph   | 15,336      | 0             | 0          |   0 |
| clique [1..100] as AdjacencyMap    | 435,872     | 31,864        | 31,864     |   0 |
| clique [1..100] as IntAdjacencyMap | 172,672     | 31,864        | 31,864     |   0 |
| clique [1..100] as Relation        | 1,667,184   | 32,256        | 32,256     |   3 |

## Tree with n! nodes

| Case                          | Total bytes | Max residency | Final live | GCs |
|-------------------------------|-------------|---------------|------------|-----|
| someTree 5 as Algebra.Graph   | 376,776     | 94,016        | 94,016     |   0 |
| someTree 5 as AdjacencyMap    | 949,352     | 81,192        | 81,192     |   1 |
| someTree 5 as IntAdjacencyMap | 912,160     | 78,680        | 78,680     |   1 |
| someTree 5 as Relation        | 951,568     | 86,696        | 86,696     |   1 |
| someTree 8 as Algebra.Graph   | 118,681,048 | 21,074,816    | 21,074,816 | 222 |
| someTree 8 as AdjacencyMap    | 313,033,272 | 16,690,992    | 16,690,992 | 595 |
| someTree 8 as IntAdjacencyMap | 299,130,792 | 16,018,720    | 16,018,720 | 570 |
| someTree 8 as Relation        | 314,402,712 | 66,201,416    | 18,444,896 | 598 |

*Warning*: The outlier of max residency for relation could be due to its NFData instance
and might not be representative of real world use.

<!-- RESULTS -->

## Compare a circuit to a clique

|Name|100|1000|
|---|---|---|
|Algebra.Graph|521.6 μs|45.89 ms|
|AdjacencyMap|518.6 μs|48.13 ms|
|IntAdjacencyMap|115.9 μs|6.641 ms|
|Relation|1608 μs|115.8 ms|

## Is a clique a subgraph of a circuit?

|Name|100|1000|
|---|---|---|
|Algebra.Graph|978.7 μs|51.90 ms|
|AdjacencyMap|506.2 μs|48.15 ms|
|IntAdjacencyMap|114.6 μs|6.598 ms|
|Relation|1638 μs|115.2 ms|

