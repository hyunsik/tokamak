# Preliminary
This documentation mostly addresses overall production design of Tokamak rather than implementation issues.

# Design Goals
* Easy to embed and interoperate general-purposed programming languages
* Look like a functional programming language which is executed across cluster nodes.
* Basically it will be an array processing system.
* Declarative language support and query optimization
  * You don't need to care about optimization, just write your logics.
* Highly parallel and distributed computing on local clusters or cloud environments which provide high-bandwidth networks.
* Easy configuration (for cloud environment and large clusters)
* Fast job execution is the first goal. Thus, other goals like fault-tolerance will be guaranteed only when they are explicitly specified.
* Primarily designed for in-memory processing, like DRAM or extremely fast non volatile memory.
* Primarily targets ~ TB sized input data
* Workload will be interactive if cluster resources are sufficiently available.

# Query and Programming Model

## Data Types
### Primitive data types for elements
* signed, unsigned integer (8, 16, 32, 64, 128 bits), floats (32, 64 bits)
* bytes
* Vector types
* Matrix types
  * D-dimentional arrays (raster data)
* Set, Bag, Tuple, Struct (== Tuple, actually, a syntactic sugar for tuple)

### Data set types
* Vector
* Matrix (D-dimentional vector)
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
* BLOB type values in external SQL or NoSQL stores
* Column storage file formats in file systems
* Column storage in block storage or object storage in Cloud

# Query Processing
(TODO)
