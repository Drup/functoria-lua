open Functoria_lua

(** User defined types *)
let char : char usertype impl = new_type "MyLua.LuaChar"
type pair
let pair : pair usertype impl = new_type "MyLua.Pair"

(** Semantics for the user defined types *)
let makelib = impl @@ object
    inherit [_]foreign "MyLua.MakeLib" (view @-> view @-> usercode)
    method connect _ _ _ = "()"
  end

(** Running application *)
let files =
  let file = Key.Arg.conv ~conv:Cmdliner.Arg.file ~runtime_conv:"Cmdliner.Arg.file"
      ~serialize:Format.pp_print_string
  in
  let doc = "Lua files to be executed by the interpreter" in
  let doc = Key.Arg.info ~docv:"FILES" ~doc ["f";"files"] in
  let key = Key.Arg.opt ~stage:`Run Key.Arg.(list file) [] doc in
  Key.create "files" key

let dump =
  let doc = "Dump the state after execution" in
  let doc = Key.Arg.info ~docv:"DUMP" ~doc ["d";"dump"] in
  let key = Key.Arg.flag ~stage:`Both doc in
  Key.create "dumpstate" key

let runinterp =
  foreign ~keys:Key.[abstract files; abstract dump] "MyLua.Run" (interp @-> job)

(** Functor spaghetti *)

let t = t3 () $ char $ pair $ iolib_type
let char_t = tv1() $ t
let pair_t = tv2() $ t
let iolib_t = tv3() $ t

let w = withtype () $ as_type t
let c =
  c5()
  $ (iolib() $ iolib_t)
  $ (camllib() $ iolib_t)
  $ (w $ strlib)
  $ (w $ mathlib)
  $ (makelib $ char_t $ pair_t)

let i =
  mk_interp $ mk_parser $ (mk_eval() $ as_type t $ c)


let () = register "toplua" [runinterp $ i]
