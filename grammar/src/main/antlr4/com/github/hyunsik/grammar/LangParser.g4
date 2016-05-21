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

package_contents
  : mod EOF
  ;

mod
  : item*
  ;

/*
===============================================================================
  Items (Top-level items in a source code)
===============================================================================
*/

item
  : visibility import_decl
  | visibility mod_decl
  | visibility type_decl
  | visibility item_fn_decl
  ;

visibility : PUB | PRIV | /*nothing*/ ;

import_decl
  : IMPORT view_path SEMI
  ;

mod_decl
  : MOD IDENT (SEMI | LBRACE item* RBRACE)
  ;


/*
===============================================================================
  View Path
===============================================================================
*/

view_path
  : non_global_path MOD_SEP LBRACE RBRACE
  | non_global_path MOD_SEP LBRACE ident_list RBRACE
  | non_global_path MOD_SEP STAR
  | non_global_path
  ;

path : MOD_SEP? non_global_path;
non_global_path : ident (MOD_SEP ident)* ;

path_with_colon_tps : path (MOD_SEP LT (generics)? GT )? ;

/*
===============================================================================
  Type Decl
===============================================================================
*/
type_decl : TYPE ident (LT (generic_decls)? GT)? EQ ty SEMI ;

/*
===============================================================================
  block
===============================================================================
*/

block
  : LBRACE import_decl* block_element* (block_last_element)? RBRACE
  ;

block_element
  : expr SEMI+
  | stmt_not_just_expr (SEMI)*
  ;

block_last_element
  : expr
  | expr_stmt
  ;

stmt_not_just_expr
  : import_decl
  | local_var
  | expr_stmt
  ;

local_var
  : (LET | VAR) local_var_decl (COMMA local_var_decl)* SEMI
  ;

local_var_decl
  : pat (COLON ty)? (EQ expr)?
  ;


/*
===============================================================================
  Fn Decl
===============================================================================
*/
item_fn_decl
  : FN ident LPAREN args? RPAREN (COLON ret_ty)? fun_body
  | FN ident LPAREN args? RPAREN COLON ret_ty EQ expr
  ;

args : arg | arg COMMA args ;
arg : pat COLON ty ;

ret_ty
 : ty
 ;

fun_body : block;

/*
===============================================================================
  exprs and statements
===============================================================================
*/

expr_stmt
  : expr_stmt_block
  | expr_stmt_not_block
  ;
expr_stmt_block : UNSAFE? block;

expr_stmt_not_block
  : expr_if
  | expr_match
  | expr_loop
  | expr_while
  ;

/*
===============================================================================
  expr_(if, while)
===============================================================================
*/

label
  : ident
  ;

expr_if
  : IF expr block (ELSE block_or_if)? ;

block_or_if : block | expr_if ;

expr_while : (label COLON)? WHILE expr block ;

expr_loop
  : (label COLON)? LOOP block
  ;

expr_match
  : MATCH expr LBRACE (match_clauses)? RBRACE
  ;

match_clauses
  : match_clause match_clauses*
  ;

match_clause
  : pats_or (IF expr)? FAT_ARROW (expr) (COMMA)?
  ;


/*
===============================================================================
  Ty
===============================================================================
*/

ty
 : LPAREN RPAREN // empty
 | LPAREN tys RPAREN // tuple
 | path (LT (generics)? GT)?
 ;

tys
 : ty (COMMA)? | ty (COMMA pats)
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
  Pat (Pattern)
===============================================================================
*/

pat
 : LPAREN RPAREN
 | LPAREN pats RPAREN
 | expr
 ;

pats
 : pat (COMMA)? | pat COMMA pats
 ;

pats_or
  : pat
  | pat OR pats_or ;

/*
===============================================================================
  Expr
===============================================================================
*/

exprs
  : expr | expr COMMA exprs
  ;

expr : expr_1 EQ expr
  | expr_1 BINOPEQ expr
  | expr_1
  ;

expr_1 : expr_1 OR expr_2
  | expr_2 ;
expr_2 : expr_2 AND expr_3
  | expr_3 ;

expr_3 : expr_3 EQEQ expr_4
  | expr_3 NE expr_4
  | expr_4 ;

expr_4 : expr_4 LT expr_5
  | expr_4 LE expr_5
  | expr_4 GE expr_5
  | expr_4 GT expr_5
  | expr_5
  ;

expr_5
  : expr_6
  ;

expr_6
  : expr_6 OR OR expr_7
  | expr_7
  ;

expr_7
  : expr_8 CARET expr_9
  | expr_9
  ;

expr_8
  : expr_8 AND expr_9
  | expr_9
  ;

expr_9
  : expr_9 (LT LT | GT GT) expr_10
  | expr_10
  ;

expr_10
  : expr_10 (PLUS | MINUS) expr_11
  | expr_11
  ;

expr_11
  : expr_11 AS ty
  | expr_12
  ;

expr_12
  : expr_12 (STAR | SLASH | PERCENT) expr_prefix
  | expr_prefix
  ;

expr_prefix
  : NOT expr_prefix
  | MINUS expr_prefix
  | expr_dot_or_call
  ;

expr_dot_or_call
  : expr_dot_or_call DOT ident (MOD_SEP LT (generics)? GT)? (LPAREN (exprs)? RPAREN)?
  | expr_dot_or_call LPAREN (exprs)? RPAREN
  | expr_dot_or_call LBRACKET expr RBRACKET
  | expr_bottom
  ;

expr_bottom
  : LPAREN (exprs (COMMA)?)? RPAREN
  | RETURN (expr)?
  | BREAK (label)?
  | path_with_colon_tps
  | expr_stmt
  | lit
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
  | UNDERSCORE
  ;


/*
===============================================================================
  Lit (Literal)
===============================================================================
*/

lit
  : TRUE
  | FALSE
  | LIT_BYTE
  | LIT_INTEGER
  | LIT_FLOAT
  | LIT_STR
  | LIT_BYTE_STR
  | LIT_BYTE_STR_RAW
  | LIT_STR_RAW
  | LPAREN RPAREN
  ;

