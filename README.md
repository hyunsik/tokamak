# What is Tokamak?
Tokamak is an analytical data processing system on scientific data sets, usually composed of multi-dimensional arrays. You can regard Tokamak is a kind of array database systems, and like MapReduce and Spark, Tokamak enables users to employ general-purposed programming languages, such as Python and Rust.

For scientific data processing, Tomakak provides primitive data types as well as very complex scientific data types, such as d-dimentional array, vector, matrix, set, bag, and tuple.

Array computing is usually computation intensive. For high throughput, Tokamak is designed to fully exploit modern hardware feature, vectorized processing in CPUs and GPGPU. Basically, it is designed to run on shared-nothing clusters whose memory is huge.
