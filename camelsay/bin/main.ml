open Core

(* TODO add flag for different camel styles *)

let camel_speech ?(message = "*spits*") () =
  let camel_body =
    {|
      //
  _oo\
  (__/ \  _  _
    \  \/ \/ \
    (         )\
      \_______/  \
      [[] [[]
      [[] [[]
    |}
  in
  let full_text = (Printf.sprintf "\n< %s >\n" message) ^ camel_body in
  print_endline full_text 

let message_param =
  (* makes sense for this to be anonymous. it's the only flag (for now), and it's the main thing one would pass in *)
  let open Command.Param in
  anon ("message" %: string)

let camelsay_command =
  Command.basic
  ~summary:"Forces the captive camel to utter a phrase of your choosing."
  (Command.Param.map message_param ~f:(fun message () ->
    camel_speech ~message ()))

let () = Command_unix.run ~version:"0.01" camelsay_command