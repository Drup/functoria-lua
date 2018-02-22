open Functoria_lua

let char : char usertype impl = new_type "Toplua.LuaChar"

type pair
let pair : pair usertype impl = new_type "Toplua.Pair"

let t = t3 () $ char $ pair $ iolib_type
let char_t = tv1() $ t
let pair_t = tv2() $ t
let iolib_t = tv3() $ t

let makelib = impl @@ object
    inherit [_]foreign "Toplua.MakeLib" (view @-> view @-> usercode)
    method connect _ _ _ = "()"
  end

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

let () = register "toplua" [runlua $ i]
