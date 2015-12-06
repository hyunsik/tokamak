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
Context contains various metadata about a Tokamak cluster instance. It includes function registry and type registry.

## System Context
A context is shared across all cluster nodes. Once loaded from the system configuration and the system constants, Context is always regarded as immutable.

Session is a set of metadata overriding context.

## Default Context

## Job Context
Job Context is a final context for each job execution. It will be shared across all Tasks for a single job.

# In-memory row data structure
In this section, we address how rows and columns are represented with in-memory data structure in Tokamak core. A minipage is a sequence of memory blocks. A single row or some column values can be stored in a minipage. As a result, either row-oriented or column-oriented approaches can be implemented with a single data structure. A page is a container for a list of minipages. So, a page represents multiple rows in the row store as well as column store.

## MiniPage details

### Fixed-length (direct) MiniPage
A fixed-length minipage implementation allows different encoding ways. Basically, uncompressed minipage is supported, and later we will implement some light-weight encoding minipages, such as:
 * Run-length encoded minipage
 * Bit-vector encoded minipage

### Variable-length MiniPage
For a variable length field or huge-sized values like BLOB, MiniPage provides an indirect way to access its data, i.e., just pointers to data are stored in minipages and each pointer indicates an actual field value. Another application of indirect minipage is dictionary encoding.

# Function System
In Tokamak, function is the first-class citizen for operations between data elements and data sets. Basic operations, user codes, explicit or implicit type castings are based on Tokamak function system. It keeps Tokamak's system design simple and consistent.

For it, a flexible function system to allow users and developers to easily add functions.

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

## Function Overloading
Tokamak provides function overloading. The same function name can be defined multiple times with different arguments.

## Platform-dependent implementation
