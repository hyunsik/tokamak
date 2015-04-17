This project aims at developing a modern data processing kernel for general purpose data processing in large clusters. It will provide various storage types like HDFS, local file system, and Amazon S3.

It has the following design considerations:

* Modern Hardware Utilization

Considerations:
 - CPU SIMD, Pipelining
 - Cache Conscious 
 - SSD

Our approaches:
 - Vectorization
 - Query Compilation (JIT)
 - Prefetch and Async I/O

* Column data processing techniques
 - Pure columnar storage layer
 - Fractured mirror against HDFS

* In-memory processing

* Coprcessors (Intel Xeon Phi and GPGPU) utilization
 - 

= Key features =

== Storage Tiering for Processing ==

Basically, it is designed to use three storage layers: distributed storage, fractured mirror, and columnar in-memory table.


Each storage layer plays the following roles:
* Distributed Storage is used as a scalable and reliable storage.

* Fractured Mirror will be the primary storage for our processing kernel. It will be optimized for data processing and is mostly equivalent to in-memory table or tuple structure. So, it allows the kernel to directly process a batch of data sets without any data (de)serialization, wasting lots of CPU cycles. Unlike distributed storages that force their own data layout or distribution, mirrors will have our own physical data layouts, specialized for efficient processing of the kernel. Also, SSD or NVMe SSD will be used for this mirror storage, and kernel will be optimized for those storages.

* Columnar In-memory Table

== How the processing kernel uses it ==

Distributed Storage <-> Fractured Mirror <-> Columnar In-memory Table
 ^                                                   ^
  \-------------------------------------------------/
