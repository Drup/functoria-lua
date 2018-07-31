module LuaChar = struct
  type 'a t    = char
  let tname    = "char"
  let eq _    = fun x y -> x = y
  let to_string   = fun _ c -> String.make 1 c 
end

module Pair = struct
  type 'a t    = 'a * 'a
  let tname    = "pair"
  let eq _    = fun x y -> x = y
  let to_string   = fun f (x,y) -> Printf.sprintf "(%s,%s)" (f x) (f y)
  let mk  x y     = (x,y)
  let fst         = fst
  let snd         = snd
end

module MakeLib 
    (CharV: Lua.Lib.TYPEVIEW with type 'a t        = 'a LuaChar.t)
    (PairV: Lua.Lib.TYPEVIEW with type 'a t        = 'a Pair.t
                              and type 'a combined = 'a CharV.combined)
  : Lua.Lib.USERCODE with type 'a userdata' = 'a CharV.combined = struct

  type 'a userdata' = 'a PairV.combined
  module M (C: Lua.Lib.CORE with type 'a V.userdata' = 'a userdata') = struct
    module V = C.V
    let ( **-> ) = V.( **-> )
    let ( **->> ) x y = x **-> V.result y
    module Map = struct
      let pair = PairV.makemap V.userdata V.projection
      let char = CharV.makemap V.userdata V.projection
    end    

    let init g = 

      C.register_module "Pair"
        [ "mk", V.efunc (V.value **-> V.value **->> Map.pair) Pair.mk
        ; "fst",V.efunc (Map.pair **->> V.value)              Pair.fst
        ; "snd",V.efunc (Map.pair **->> V.value)              Pair.snd
        ] g;

      C.register_module "Char"
        [ "mk", V.efunc (V.string **->> Map.char)
            (function 
              | "" -> C.error "Char.mk: empty string"   
              | s  -> s.[0]
            )
        ] g;        

      C.register_module "Example" 
        ["argv",   (V.list V.string).V.embed (Array.to_list Sys.argv);
         "getenv", V.efunc (V.string **->> V.string) Sys.getenv;
        ] g;


  end (* M *)
end (* MakeLib *)

module Run (I : Lua.INTERP) = struct
  module V = I.Value
  let state = I.mk ()
  let showresults =
    let rec loop n = function
      | h :: t -> print_string "Result "; print_int n; print_string " = ";
        print_endline (V.to_string h); loop (n+1) t
      | [] -> ()
    in loop 1
  let run infile = ignore (I.dofile state infile)
  let run_interactive infile =
    let rec loop n pfx =
      let line = input_line infile in
      if String.length line > 0 && String.get line (String.length line - 1) = '\\' then
        loop n (pfx ^ String.sub line 0 (String.length line - 1) ^ "\n")
      else
        begin
          ignore (I.dostring state (pfx ^ line ^ "\n"));
          flush stdout; flush stderr;
          loop (n+1) ""
        end
    in  try loop 1 "" with End_of_file -> ()
  
  let start () =
    begin match Key_gen.files () with
      | [] -> run_interactive stdin
      | l -> List.iter run l
    end ;
    if Key_gen.dumpstate () then begin
      print_endline "final state: ";
      Luahash.iter (fun k v -> print_string "  ";
                     print_string (V.to_string k); print_string " |-> ";
                     print_endline (V.to_string v)) state.V.globals
    end
end


