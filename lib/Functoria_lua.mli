[@@@warning "-32"]
module Key = Functoria_key
include Functoria_app.DSL

type +'a usertype
val usertype : 'a usertype typ
val new_type : 
  ?packages:package list ->
  ?keys:key list ->
  ?deps:abstract_impl list -> string -> 'a usertype impl
    
type +'a luavalue
val value : 'a luavalue typ
val mk_value : unit -> ('a usertype -> 'a luavalue) impl

type ast
val ast : ast typ
val mk_ast : unit -> ('a luavalue -> ast) impl

type parser
val parser : parser typ
val maker : (ast -> parser) typ
val mk_parser : (ast -> parser) impl

type barecode
val barecode : barecode typ
val mathlib : barecode impl
val strlib : barecode impl

type +'a usercode
val usercode : 'a usercode typ
val c2 : unit -> ('a usercode -> 'a usercode -> 'a usercode) impl
val c3 : unit -> ('a usercode -> 'a usercode -> 'a usercode -> 'a usercode) impl
val c4 : unit -> ('a usercode -> 'a usercode -> 'a usercode -> 'a usercode -> 'a usercode) impl
val c5 : unit -> ('a usercode -> 'a usercode -> 'a usercode -> 'a usercode -> 'a usercode -> 'a usercode) impl
  
type +'a combined = private 'a usertype
val combined : 'a combined typ

val as_type : 'a combined impl -> 'a usertype impl

type (+'a, +'b) view
val view : ('a, 'b) view typ

val withtype : unit -> ('a usertype -> barecode -> 'a usercode) impl

val tv1 : unit -> ((< tv1 : 'a ; .. > as 'c) combined -> ('a, 'c) view) impl
val tv2 : unit -> ((< tv2 : 'a ; .. > as 'c) combined -> ('a, 'c) view) impl
val tv3 : unit -> ((< tv3 : 'a ; .. > as 'c) combined -> ('a, 'c) view) impl
val tv4 : unit -> ((< tv4 : 'a ; .. > as 'c) combined -> ('a, 'c) view) impl

val t2 : unit -> ('a usertype -> 'b usertype -> < tv1 : 'a; tv2 : 'b > combined) impl
val t3 : unit ->
           ('a usertype ->
            'b usertype ->
            'c usertype -> < tv1 : 'a; tv2 : 'b; tv3 : 'c > combined)
           impl
val t4 : unit ->
           ('a usertype ->
            'b usertype ->
            'c usertype ->
            'd usertype ->
            < tv1 : 'a; tv2 : 'b; tv3 : 'c; tv4 : 'd > combined)
           impl

type iolib
val iolib_type : iolib usertype impl
val iolib : unit -> ((iolib, 'a) view -> 'a usercode) impl
val camllib : unit -> ((iolib, 'a) view -> 'a usercode) impl

type eval
val eval : eval typ
val mk_eval : unit -> ('a usertype -> 'a usercode -> eval) impl

type interp
val interp : interp typ
val mk_interp : ((ast -> parser) -> eval -> interp) impl

val runlua : (interp -> job) impl

val register:
  ?keys:Key.t list ->
  ?packages:Functoria.package list -> string -> job impl list -> unit

(**/**)

val run: unit -> unit


(*
 * Copyright (c) 2018 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
