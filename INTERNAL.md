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

# JIT Compilation
In Tokamak, JitManager generates some JIT code fragments in runtime for
arithmetic, comparison, other logical expressions and predicates.

Access to nested fields

* Various invocation methods
  * LLVM intrinsics
  * Rust function
  * Python function (including lambda)
  * External functions via C FFI and JNI
* UDF from user's JARs or dynamic libraries
* A simple way to map from external functions to Tokamak functions
  * Map from LLVM intrinsics to Tokamak function
  * Map from external functions (C or Java functions) to Tokamak function
* LLVM bitcode or IR execution as a function in runtime

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
