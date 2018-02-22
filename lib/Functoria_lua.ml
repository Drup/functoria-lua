open Rresult
open Astring

module Key = Functoria_key
module Name = Functoria_app.Name
module Codegen = Functoria_app.Codegen
include Functoria


let tool_name = "functoria-lua"

let src = Logs.Src.create tool_name ~doc:"functoria-lua cli tool"
module Log = (val Logs.src_log src : Logs.LOG)

(** Devices **)

class base = object
  method packages: package list Key.value = Key.pure []
  method keys: Key.t list = []
  method connect (_:Info.t) (_:string) (_l: string list) = "()"
  method configure (_: Info.t): (unit, R.msg) R.t = R.ok ()
  method build (_: Info.t): (unit, R.msg) R.t = R.ok ()
  method clean (_: Info.t): (unit, R.msg) R.t = R.ok ()
  method deps: abstract_impl list = []
end

type +'a usertype = USERTYPE
let usertype = Type USERTYPE

class ['ty] new_type
     ?(packages=[]) ?(keys=[]) ?(deps=[]) module_name
  : ['ty usertype] configurable
  =
  let name = Name.create module_name ~prefix:"type" in
  object
    method ty : 'ty usertype typ = usertype
    method name = name
    method module_name = module_name
    method keys = keys
    method packages = Key.pure packages
    method connect (_:Info.t) (_:string) (_l: string list) = "()"
    method clean _ = R.ok ()
    method configure _ = R.ok ()
    method build _ = R.ok ()
    method deps = deps
  end
let new_type ?packages ?keys ?deps module_name =
  impl (new new_type ?packages ?keys ?deps module_name)


(* Luavalue.S *)
type _ luavalue = VALUE
let value = Type VALUE
let mk_value () = impl @@ object
    inherit base
    method ty = usertype @-> value
    method name = "ast"
    method module_name = "Luavalue.Make"
  end

(* Luaast.S *)
type ast = AST
let ast = Type AST
let mk_ast () = impl @@ object
    inherit base
    method ty = value @-> ast
    method name = "ast"
    method module_name = "Luaast.Make"
  end

(* Luaparser.S *)
type parser = PARSER
let parser = Type PARSER
let maker = ast @-> parser

let mk_parser = impl @@ object
    inherit base
    method ty = maker
    method name = "parser"
    method module_name = "Luaparser.MakeStandard"
  end

(* Lualib.BARECODE *)
type barecode = BARECODE
let barecode = Type BARECODE

let bc n mn = impl @@ object
    inherit base
    method ty = barecode
    method name = n
    method module_name = mn
  end

let mathlib = bc "mathlib" "Luamathlib.M"
let strlib = bc "strlib" "Luastrlib.M"

(* Lualib.USERCODE *)
type +'a usercode = USERCODE
let usercode = Type USERCODE

let c2 () = impl @@ object
    inherit base
    method ty = usercode @-> usercode @-> usercode
    method name = "combineC2"
    method module_name = "Lua.Lib.Combine.C2"
  end
let c3 () = impl @@ object
    inherit base
    method ty
      = usercode @-> usercode @-> usercode @-> usercode
    method name = "combineC3"
    method module_name = "Lua.Lib.Combine.C3"
  end
let c4 () = impl @@ object
    inherit base
    method ty
      = usercode @-> usercode @-> usercode @-> usercode @-> usercode
    method name = "combineC4"
    method module_name = "Lua.Lib.Combine.C4"
  end
let c5 () = impl @@ object
    inherit base
    method ty
      = usercode @-> usercode @-> usercode @-> usercode @-> usercode @-> usercode
    method name = "combineC5"
    method module_name = "Lua.Lib.Combine.C5"
  end

(* Types *)

type +'a combined = 'a usertype
let combined = usertype

let as_type x = x

type (+'a, +'b) view = VIEW
let view = Type VIEW

let withtype () = impl @@ object
    inherit base
    method ty = usertype @-> barecode @-> usercode
    method name = "withtype"
    method module_name = "Lua.Lib.WithType"
  end

let takeview i =
  proj (combined @-> view) ("TV"^string_of_int i)
let tv1 () : ((< tv1 : 'a ; .. > as 'c) combined -> ('a, 'c) view) impl = takeview 1
let tv2 () : ((< tv2 : 'a ; .. > as 'c) combined -> ('a, 'c) view) impl = takeview 2
let tv3 () : ((< tv3 : 'a ; .. > as 'c) combined -> ('a, 'c) view) impl = takeview 3
let tv4 () : ((< tv4 : 'a ; .. > as 'c) combined -> ('a, 'c) view) impl = takeview 4

let t2 () = impl @@ object
    inherit base
    method ty = usertype @-> usertype @-> combined
    method name = "combine2"
    method module_name = "Lua.Lib.Combine.T2"
  end
let t3 () = impl @@ object
    inherit base
    method ty = usertype @-> usertype @-> usertype @-> combined
    method name = "combine3"
    method module_name = "Lua.Lib.Combine.T3"
  end
let t4 () = impl @@ object
    inherit base
    method ty = usertype @-> usertype @-> usertype @-> usertype @-> combined
    method name = "combine4"
    method module_name = "Lua.Lib.Combine.T4"
  end

(* Luaiolib *)
type iolib
let iolib_type : iolib usertype impl = new_type "Luaiolib.T"
let iolib () = impl @@ object
    inherit base
    method ty = view @-> usercode
    method name = "iolib"
    method module_name = "Luaiolib.Make"
  end

(* Luacamllib *)
let camllib () = impl @@ object
    inherit base
    method ty = view @-> usercode
    method name = "camllib"
    method module_name = "Luacamllib.Make"
  end

(* Lua.MakeEval *)
type eval = EVAL
let eval = Type EVAL
let mk_eval () = impl @@ object
    inherit base
    method ty = usertype @-> usercode @-> eval
    method name = "eval"
    method module_name = "Lua.MakeEval"
  end

type interp = INTERP
let interp = Type INTERP
let mk_interp = impl @@ object
    inherit base
    method ty = maker @-> eval @-> interp
    method name = "interp"
    method module_name = "Lua.MakeInterp"
  end

let runlua = impl @@ object
    inherit base
    method ty = interp @-> job
    method name = "run"
    method module_name = "Lua.Run"
    method! connect _ _ _ = "fun () -> ()"
  end

(** Tool-related functions *)

let with_output ?mode f k err =
  match Bos.OS.File.with_oc ?mode f k () with
  | Ok b -> b
  | Error _ -> R.error_msg ("couldn't open output channel for " ^ err)

(** Makefile **)
    
let configure_makefile ~app_name info =
  let name = Info.name info in
  let open Codegen in
  let file = Fpath.(v "Makefile") in
  with_output file (fun oc () ->
      let fmt = Format.formatter_of_out_channel oc in
      append fmt {|
# %s
-include Makefile.user";
OPAM = opam
BIN = %s
DEPEXT ?= opam depext --yes --update %s

.PHONY: all depend depends clean build
all:: build

depend depends::
	$(OPAM) pin add --no-action --yes %s .
	$(DEPEXT)
	$(OPAM) install --yes --deps-only %s
	$(OPAM) pin remove --no-action %s

build::
	$(BIN) build

clean::
	$(BIN) clean
|}
        (generated_header ()) name app_name app_name app_name app_name;
      R.ok ())
    "Makefile"
let clean_makefile () = Bos.OS.File.delete Fpath.(v "Makefile")

(** Ocamlbuild *)

let fn = Fpath.(v "myocamlbuild.ml")

let configure_myocamlbuild () =
  Bos.OS.File.exists fn >>= function
  | true -> R.ok ()
  | false -> Bos.OS.File.write fn ""

(* we made it, so we should clean it up *)
let clean_myocamlbuild () =
  match Bos.OS.Path.stat fn with
  | Ok stat when stat.Unix.st_size = 0 -> Bos.OS.File.delete fn
  | _ -> R.ok ()

(** OPAM file *)
           
let configure_opam ~app_name info =
  let name = Info.name info in
  let open Codegen in
  let file = Fpath.(v name + "opam") in
  with_output file (fun oc () ->
      let fmt = Format.formatter_of_out_channel oc in
      append fmt "# %s" (generated_header ());
      Info.opam ~name:app_name fmt info;
      append fmt "build: [ \"%s\" \"build\" ]" tool_name;
      append fmt "available: [ ocaml-version >= \"4.03.0\" ]";
      R.ok ())
    "opam file"

let clean_opam ~name = Bos.OS.File.delete Fpath.(v name + "opam")


let app_name name =
  String.concat ~sep:"-" ["lua" ; "ml" ; name]

let configure i =
  let name = Info.name i in
  Log.info (fun m -> m "Configuring.");
  let app_name = app_name name in
  configure_myocamlbuild () >>= fun () ->
  configure_opam ~app_name i >>= fun () ->
  configure_makefile ~app_name i

let clean i =
  let name = Info.name i in
  clean_myocamlbuild () >>= fun () ->
  clean_makefile () >>= fun () ->
  clean_opam ~name >>= fun () ->
  Bos.OS.File.delete Fpath.(v "main.native.o") >>= fun () ->
  Bos.OS.File.delete Fpath.(v "main.native") >>= fun () ->
  Bos.OS.File.delete Fpath.(v name)

(** Compilation *)


let terminal () =
  let dumb = try Sys.getenv "TERM" = "dumb" with Not_found -> true in
  let isatty = try Unix.(isatty (descr_of_out_channel Pervasives.stdout)) with
    | Unix.Unix_error _ -> false
  in
  not dumb && isatty

let compile i =
  let libs = Info.libraries i in
  let tags =
    [ "warn(A-4-41-42-44)";
      "debug";
      "bin_annot";
      "strict_sequence";
      "principal";
      "safe_string" ] @
    (if terminal () then ["color(always)"] else [])
  in
  let concat = String.concat ~sep:"," in
  let cmd = Bos.Cmd.(v "ocamlbuild" % "-use-ocamlfind" %
                     "-classic-display" %
                     "-tags" % concat tags %
                     "-pkgs" % concat libs %
                     "main.native")
  in
  Log.info (fun m -> m "executing %a" Bos.Cmd.pp cmd);
  Bos.OS.Cmd.run cmd

let link info =
  let name = Info.name info in
  Bos.OS.Cmd.run Bos.Cmd.(v "ln" % "-nfs" % "_build/main.native" % name)
  >>= fun () -> Ok name

let build i =
  compile i >>= fun () ->
  link i >>| fun out ->
  Log.info (fun m -> m "Build succeeded: %s" out)
 
module Project = struct
  let name = "functoria-lua"
  let version = "%%VERSION%%"
  let prelude = {|
let (>>=) x f = f x
let return x = x
let run () = ()
|}

  (* The ocamlfind packages to use when compiling config.ml *)
  let packages = [package "functoria-lua"]

  (* The directories to ignore when compiling config.ml *)
  let ignore_dirs = []

  let create jobs = impl @@ object
      inherit base_configurable
      method ty = job
      method name = tool_name
      method module_name = "Functoria_lua_runtime"
      method! keys = [
      ]
      method! packages =
        let common = [
          package "lua-ml";
          package "functoria-runtime";
          package ~build:true "ocamlfind" ;
          package ~build:true "ocamlbuild" ;
        ] in
        Key.pure common

      method! build = build
      method! configure = configure
      method! clean = clean
      method! connect _ _mod _names = "()"
      method! deps = List.map abstract jobs
    end

end

include Functoria_app.Make (Project)


let register
    ?keys ?packages
    name jobs =
  let argv = Functoria_app.(keys sys_argv) in
  let init = [ argv ] in
  register ?keys ?packages ~init name jobs

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
