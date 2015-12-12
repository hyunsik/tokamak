# What is Tokamak?
Tokamak is an analytical data processing system on scientific data sets,
usually composed of multi-dimensional arrays. Simply, You can regard that
Tokamak is a kind of array DBMS [1], but like MapReduce and Spark, Tokamak
enables users to employ general-purposed programming languages, such as
Python and Rust.

For scientific data processing, Tokamak basically provides primitive data
types as well as very complex scientific data types, such as d-dimentional
array, vector, matrix, set, bag, and tuple. You can easily store and handle
data items, such as image, spatial-temporal data item, graph, DNA-sequencers,
sensor data, statistic data.

Manipulating arrays is usually computation intensive. To achieve high throughput
and low latency, Tokamak employs in-memory parallel and distributed processing
as its fundamental principle. In addition, Tokamak is designed to fully leverage
the features of modern hardwares and vectorized processing in CPUs and GPU.
In order to access complex data element in data set and get rid of interpreter
overhead of query processing, Tokamak exploits JIT compilation techniques
through LLVM. In other words, Tokamak works like the compilers of programming
languages but runs on large clusters.

## See Also
 1. [Array DBMS](https://en.wikipedia.org/wiki/Array_DBMS)

# Build
*Requirements*
 * LLVM (3.7 or higher) with Clang
 * RUST (1.6-nightly or higher)

*Building*
```
git submodule init
git submodule update
cargo build --release
```

### Building LLVM (if necessary)
If your OS distribution does not provide llvm-3.6-dev package, you may need to manually compile LLVM. Please refer to http://clang.llvm.org/get_started.html.
