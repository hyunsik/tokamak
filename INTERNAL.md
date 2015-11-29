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

# Function System

In Tokamak, function is the everything for operations between data elements and data sets. Basic operations, user programmed code, explicit or implicit type casting all are based on Tokamak function system. It keeps Tokamak simple with the system design consistency.

For it, we need abundant functions and a flexible function system to allow users and developers to easily add functions.

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
