# Terminologies
All terminologies are the unique names for clear understanding.

## api
* DataSet - It can be used for both an input source and output result.
* DataSource - Is means an input data source
* DataFrame - A collection of APIs to transform and manipulate data sets

## exec
* Executor

## rows
* Page
* MiniPage

# Cluster Leader
cluster management metadata, user session registry,

# Context and Session
Context contains various metadata about a Tokamak cluster instance. It includes function registry and type registry.

## System Context
 Context is shared across all cluster nodes. Once loaded from the system configuration and the system constants, Context is always kept as immutable.

Session is a set of metadata overriding context.

## Default Context

## Job Context
Job Context is a final context for each job execution. It will be shared across all Tasks for a single job.

# In-memory row data structure
In this section, we address how rows and columns are represented through in-memory data structure in Tokamak core. A minipage is just a sequence memory block. A single row or some column values can be stored in a minipage. We can implement row-oriented or column-oriented approaches with the a single data structure. A page is a container for a list of minipages. So, a page represents multiple rows in the row store as well as column store.

## MiniPage details

### Fixed-length (direct) MiniPage
A fixed-length minipage implementation allows different encoding ways. Basically, uncompressed minipage is supported, and later we will implement some light-weight encoding minipages, such as:
 * Run-length encoded minipage
 * Bit-vector encoded minipage

### Variable-length MiniPage
For a variable length field or huge-sized values like BLOB, MiniPage supports indirect minipage, just storing pointers in the minipage and each pointer indicates an actual field value. Another application of indirect minipage is dictionary encoding.

# Function System
In Tokamak, function is the everything for operations between data elements or data sets. Basic operations, user code, explicit or implicit type casting all are based on Tokamak function system. It keeps Tokamak simple with the system design consistency.

For it, a flexible function system to allow users and developers to easily add functions.

* Various invocation methods
  * LLVM intrinsics
  * Rust function
  * Python function (including lambda)
  * External functions via C FFI and JNI
* UDF from user's JAR or dynamic libraries
* A simple way to map from external functions to Tokamak functions
  * Map from LLVM intrinsics to Tokamak function
  * Map from external functions (C or Java functions) to Tokamak function
* LLVM bitcode or IR execution as a function in runtime

## Function Overloading
Tokamak provides function overloading. The same function name can be defined multiple times with different arguments.

## Platform-dependent implementation
