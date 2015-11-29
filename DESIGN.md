# Preliminary
This documentation mostly address the overall production design of Tokamak  rather than the implementation issues.

# Design Goals
* Easy to embed and interoperate general-purposed programming languages
* Look like a functional programming language which are executed across cluster nodes.
* Basically it will be an array computing system.
* Declarative language support and query optimization
  * You don't need to think about optimization, just write your logics.
* Highly parallel and distributed computing in local clusters or cloud environments which provide high bandwidth networks.
* Easy configuration (for cloud environment and large clusters)
* Only if necessary, fault tolerance will be guaranteed. Otherwise, it finishes jobs as faster as possible.
* Primarily designed for in-memory storages, like DRAM or extremely fast non volatile memory.
* Primarily targets ~ TB sized input data
* Workload will be interactive if cluster resources are available.

# Query and Programming Model

## Data Types
### Primitive data types for elements
* signed, unsigned integer (8, 16, 32, 64, 128 bits), floats (32, 64 bits)
* bytes
* Vector types
* Matrix types
  * D-dimentional arrays (raster data)
* Set, Bag, Tuple, Struct (== Tuple, actually, a syntactic sugar of tuple)

### Data set types
* Vector
* Matrix (N-dimentional vector)
* Set
* Bag
 * A bag of tuples == a relational table

## Operations
* data set operations: All possible operations between
  * data set vs data set
  * data set vs primitive data
  * primitive data

## User Interfaces
### Extended SQL
(TODO)
### Functional language APIs
(TODO)

# Physical materialization model

## Physical format for data sets
* In-memory row or columnar tables
* BLOB type values in external SQL or NoSQL storages
* Column storage file formats in file systems
* Column storage in block storage or object storage in Cloud

# Query Processing
