open Core

let camel_speech ~message =
  let camel_body =
    {|
  \
   \
     //
   _oo\
  (__/ \ _  _
    \  \/ \/ \
    (         )\
     \_______/  \
      [[] [[]
      [[] [[]
    |}
  in
  let full_text = (Printf.sprintf "\n< %s >\n" message) ^ camel_body in
  print_endline full_text 

let get_message = function
| Some m -> m
| None -> "*spits*"

let camelsay_command =
  Command.basic
  ~summary:"Forces the captive camel to utter a phrase of your choosing."
  (let%map_open.Command message =
    anon (maybe ("message" %: string))
  in 
  fun () -> camel_speech ~message:(get_message message))

let () = Command_unix.run ~version:"0.01" camelsay_command