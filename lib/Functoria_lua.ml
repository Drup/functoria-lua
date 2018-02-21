open Rresult
open Astring

module Key = Functoria_key
module Name = Functoria_app.Name
module Codegen = Functoria_app.Codegen
include Functoria


let tool_name = "functoria-lua"

let src = Logs.Src.create tool_name ~doc:"functoria-lua cli tool"
module Log = (val Logs.src_log src : Logs.LOG)

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
      append fmt "build: [ \"%s\" \"build\" ]" name;
      append fmt "available: [ ocaml-version >= \"4.03.0\" ]";
      R.ok ())
    "opam file"

let clean_opam ~app_name = Bos.OS.File.delete Fpath.(v app_name + "opam")


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
  clean_opam ~app_name:(app_name name) >>= fun () ->
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
  and cflags = [ "-g" ]
  and lflags = [] in
  let concat = String.concat ~sep:"," in
  let cmd = Bos.Cmd.(v "ocamlbuild" % "-use-ocamlfind" %
                     "-classic-display" %
                     "-tags" % concat tags %
                     "-pkgs" % concat libs %
                     "-cflags" % concat cflags %
                     "-lflags" % concat lflags %
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
  let prelude =
    "let return x = x\n\
     let run f = f ()"

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
