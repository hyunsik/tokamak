## Modules

This page describes the modules and their dependencies. This documentation has been written for developers who want to start contributing to this project.

* ``api`` provides user-level API for Tokamak library.
* ``common`` mostly provides abstracted interfaces, data structures used in most modules, and utilities.
* ``engine`` -
* ``func`` provides the function interface and some builtin functions.
* ``rows`` provides containers for rows, matrix, and vectors.
* ``sql`` provides data types, operators, and executors for SQL processing.
* ``storage`` provides input source interfaces, and some builtin implementations like local file system.

### Dependencies
```
api
 |--- common
 |--- sql
 |--- engine
 
common

engine

func

rows

sql
```
