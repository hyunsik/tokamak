# Terminologies
All terminologies are the unique names for clear understanding.

## api
* DataSet - A set of data which can be used for both an input source and output result.
* DataSource - An input data source
* DataFrame - A collection of APIs to transform and manipulate data sets

## exec
* Executor

## rows
* Page
* MiniPage

# Cluster Leader
Cluster management, metadata management, user session registry, and so on.

# Context and Session
Context contains various metadata about a Tokamak cluster instance. It
includes function registry and type registry.

## System Context
A context is shared across all cluster nodes. Once loaded from the system
configuration and the system constants, Context is always regarded as immutable.

Session is a set of metadata overriding context.

## Default Context

## Job Context
Job Context is a final context for each job execution. It will be shared
across all Tasks for a single job.

# In-memory row data structure
In this section, we address how rows and columns are stored in in-memory data
structures in Tokamak core. A minipage is a sequence of memory blocks.
A single row or some column values can be stored in a minipage. As a result,
either row-oriented or column-oriented approaches can be implemented with a
single data structure. A page is a container for a list of minipages. So,
a page represents multiple rows in row stores as well as column stores.

## Page

### Page Life Cycle
 * Each is created at each operator and the life cycle will be the same to the operator.
 * Operators can be categorized into two types
   * One type will write new values into Page
     * e.g., Project, Groupby, Join, Window, Sort
   * Another type will just bypass the page
     * Filter, Union

#### Per operator
*Notation*
 * O - Owner
 * N - Not Owner
 * D - Direct pass of some chunks without rearrange rows

*Operators*
 * Scan (O)
 * Filter, Having (N)
 * Project - (N, D)
 * Aggregate, Dist Aggregation - Page Owner (O)
 * Partition - Page Owner (Hash Partitioned)
 * HashJoin, Merge Join - Page Owner
 * Insert - Owner
 * Sort - Owner (Sort Buffer)
 * Union - Not Owner


## MiniPage details

### Fixed-length (direct) MiniPage
A fixed-length minipage implementation allows different encoding ways.
Basically, uncompressed minipage is supported, and later we will implement
some light-weight encoding minipages, such as:
 * Run-length encoded minipage
 * Bit-vector encoded minipage

### Variable-length MiniPage
For a variable length field or huge-sized values like BLOB, MiniPage provides
an indirect way to access its data, i.e., just pointers to data are stored in
minipages and each pointer indicates an actual field value. Another application
of indirect minipage is dictionary encoding.

## How To Write Data into MiniPage
We need to use a writer or builder to write some data. It is necessary 
because we should support various column encoding. 
Jit can make it efficient by using inline functions.

### Write Builder vs. Write Function
Page and MiniPage are opaque pointers from the developer point of 
views. Page and MiniPage include raw pointers. We need to use some approach to avoid
handling raw pointers to suppress error prone routines.

There may be two approaches: Writer or Write Function. 

#### The Pros and Cons
 Writer: Easy to keep state, but not good for inlining
 Write function : Easy for inlining, but State must be kept outside, probably making Jit complexity higher.  

# JIT Compilation
In Tokamak, JitManager generates some JIT code fragments in runtime for
arithmetic, comparison, other logical expressions and predicates.

Access to nested fields

Sort comparator

Field projection in scanner

Serialization/Deserialization for tuples

Function adapter (some emulation of reflection)

# Function System
In Tokamak, function is the first-class citizen for operations between data
elements and data sets. That is, every operations including arithmetic,
comparison, other complex operations between data elements or data sets are
implemented with functions. Builtin operations, user codes, explicit or implicit
type castings are based on Tokamak function system. It keeps Tokamak's system
design simple and consistent.

## Design Conderations
### Design Goals
* Easy to use user-defined functions: it should be easy for users to
implement and embed user codes to their processing jobs. Also, we should
hide low-level stuff and unsafe parts from users.
* Less overhead: virtual call and (de)serialization from/to tuples can cause
the significant overhead, especially an in-memory proessing system. We need
to keep its overhead less or zero if possible.

### Design Decisions
To achieve the goals, we did the following decisions:

* Various Language binding support: C, Rust, Java, and Python function
implementation should be supported for abundant functions. Depending on their
purpose, users can use high-level abstract programming languages
(e.g., Python) familar with data analysts. They also can reuse existing
libraries for their processing jobs. Besides, users can use C or Rust for
high thoughput operations.
* LLVM-based JIT: basically, JIT provides seamless and zero cost ways to bind
the internal codes with external user functions. Besides, JIT gives abundant
optimization opportunities, such as function inlining, platform-dependent
instrinsics, and branch elimination.

## Supported function bindings
* LLVM intrinsics (builtin function only)
* Rust function
* Python function, including lambda
* External functions via C FFI and JNI

## Function Declarations
Function declarations for Tokamak must be available to LLVM JIT engine.
In other words, a function declaration should be one of them:
* One of the ABIs compatable with LLVM calling convention.
* Direct IR generation

(TODO - code examples will be in following sections)
### LLVM intrinsics
 * IR generation

### Rust
 * IR generation via Rustc IR emission or extern function

### Python function
 * CPython binding or IR generation (Numba)

### C function
 * IR generation via Clang IR emission, or extern function

### Java function
 * JNI


## Function Registration

## Function Overloading
Tokamak provides function overloading. The same function name can be defined
multiple times with different arguments.
