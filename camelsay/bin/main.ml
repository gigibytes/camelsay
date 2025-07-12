open Core

let render_output ~message ~body_type =
  (* idk how to get unicode actually rendering so this is here as a regular string for now. doubt this works *)
  let emoji_camel = "U+1F42B"
  in
  let ascii_camel_body =
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
  let camel_body =
    match body_type with
    | `Emoji -> emoji_camel
    | `Ascii -> ascii_camel_body
  in
  let camel_and_speech = (Printf.sprintf "\n< %s >\n" message) ^ (camel_body) in
  print_endline camel_and_speech

let get_message = 
  function
| Some m -> m
| None -> "*spits*"
;;

let get_camel_type emoji_flag = if emoji_flag then `Emoji else `Ascii;;

let camelsay_command =
  Command.basic
  ~summary:"Forces the camel to utter a phrase of your choosing."
  [%map_open.Command
    let message = anon (maybe ("message" %: string))
    and
    emoji = flag "emoji" Command.Flag.no_arg ~doc:"Display an emoji camel instead of the default ascii camel"
    in 
    (* get_camel_type should return one of the variants that I defined in camel_body *)
    fun () -> render_output ~message:(get_message message) ~body_type:(get_camel_type emoji)
   ]
    

let () = Command_unix.run ~version:"0.01" camelsay_command