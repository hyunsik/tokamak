/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

parser grammar LangParser;

options {
	language=Java;
	tokenVocab=LangLexer;
}

@header {
}

@members {
}

/*
===============================================================================
  Package (Start Symbol)
===============================================================================
*/

pkg
  : item_import* item_list EOF
  ;

/*
pkg_attr_list
  :
  ;

pkg_attr
  :
  ;
*/

/*
===============================================================================
  Items (Top-level items in a source code)
===============================================================================
*/

item_list
  : item*
  ;

item
  : visibility mod_decl
  | visibility type_decl
  ;

mod_decl
  : MOD IDENT (SEMI | LBRACE item_list RBRACE)
  ;

item_import
  : IMPORT view_path SEMI
  ;

visibility : PUB | PRIV | /*nothing*/ ;

/*
===============================================================================
  View Path
===============================================================================
*/

view_path
  : non_global_path MOD_SEP LBRACE ident_list (COMMA)? RBRACE
  | non_global_path MOD_SEP STAR
  | non_global_path
  ;

path : MOD_SEP? non_global_path;
non_global_path : ident (MOD_SEP ident)* ;

/*
===============================================================================
  Type Decl
===============================================================================
*/
type_decl : TYPE ident (LT (generic_decls)? GT)? EQ ty SEMI ;

/*
===============================================================================
  Fn Decl
===============================================================================
*/
item_fn_decl: FN ident LPAREN RPAREN ret_ty fun_body;

ret_ty
 : ty
 | /* nothing */
 ;

fun_body : LBRACE item_import* block_element* (block_last_element)? RBRACE ;

block_element
  : LPAREN ty RPAREN
  ;

block_last_element
  : LPAREN RPAREN
  ;


/*
===============================================================================
  Ty
===============================================================================
*/

ty
 : LPAREN RPAREN // empty
 | LPAREN ty (COMMA)? RPAREN // tuple
 | LPAREN tys RPAREN // tuple
 | path (LT (generics)? GT)?
 ;

tys
 : ty (COMMA tys)?
 ;

generic_decls
 : type_params
 ;

generics
  : tys
  ;

type_params
 : type_param (COMMA type_param)*
 ;

type_param
 : ident
 ;

/*
===============================================================================
  Ident
===============================================================================
*/

ident_list: ident (COMMA ident)*;

ident
  : IDENT
  | SELF
  ;


