# What is Tokamak?
Tokamak is an analytical data processing system on scientific data sets, usually composed of multi-dimensional arrays. You can regard that Tokamak is a kind of array DBMS [1], but like MapReduce and Spark, Tokamak enables users to employ general-purposed programming languages, such as Python and Rust.

For scientific data processing, Tomakak basically provides primitive data types as well as very complex scientific data types, such as d-dimentional array, vector, matrix, set, bag, and tuple. You can easily store and handle data items, such as image, spatial-temporal data item, graph, sensor data, statistic data.

Array computing is usually computation intensive. For high throughput, Tokamak basically is based on in-memory parallel and distributed computing. In addition, Tokamak is designed to fully exploit modern hardware feature, vectorized processing in CPUs and GPGPU. In order to access complex data element in data set and get rid of interpreter overhead of query processing, Tokamak exploits JIT compilation techniques through LLVM. In other words, Tokamak works like the compilers of programming languages but runs in large clusters.

## See Also
 1. [Array DBMS](https://en.wikipedia.org/wiki/Array_DBMS)
