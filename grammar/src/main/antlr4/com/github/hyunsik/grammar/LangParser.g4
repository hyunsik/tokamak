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
  : item_list EOF
  ;

/*
pkg_attr_list
  :
  ;

pkg_attr
  :
  ;
*/

item_list
  : item*
  ;

item
  : mod_item
  ;

mod_item
  : (PUB)? MOD (mod_item_file | item_mod_group)
  ;

mod_item_file
  : IDENT SEMI
  ;

item_mod_group
  : LBRACE item_list RBRACE
  ;