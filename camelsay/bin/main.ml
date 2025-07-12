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
;;


let get_camel_type emoji_flag = if emoji_flag then `Emoji else `Ascii;;

let get_translation ~message ~language_code = 
(* this is where we reach out to whatever API i'm using for this *)
 if (Option.is_some language_code) then
    message ^ "this message has been translated :)"
  else message
;;

let translate ~message ~language =
  let language_code =
    (* TODO: support non roman chars and accents with unicode *)
    match language with
    | Some l when String.Caseless.equal l "es" -> "es_SP"
    | None | _ -> "no_op"
  in
  let translation_note =
    match language_code with
    | "no_op" -> "No translation performed, language not recognized"
    | lang -> [%string "Translated message to %{lang}"]
  in
  let maybe_translated_message =
    if (not (String.equal language_code "no_op")) then (get_translation ~message ~language_code) else message
  in
  maybe_translated_message ^ translation_note
;;

let get_message ~message ~translation_target_lang = 
  let default_message = "*spits*"
  in
  match message with
  | Some m when (Option.is_some translation_target_lang) -> translate ~message:m ~language:translation_target_lang
  (* Don't translate when the translation_target_lang option is None/when guard eq false *)
  | Some m -> m
  | None -> default_message
;;

let camelsay_command =
  Command.basic
  ~summary:"Forces the camel to utter a phrase of your choosing."
  [%map_open.Command
    let message = anon (maybe ("message" %: string))
    and
    emoji = flag "emoji" Command.Flag.no_arg ~doc:"Display an emoji camel instead of the default ascii camel"
    and
    (* Should offer a set of options to start *)
    translation_target_lang = flag "translate" (optional string) ~doc:"Translate to a different language"
    in 
    (* get_camel_type should return one of the variants that I defined in camel_body *)
    fun () -> render_output ~message:(get_message ~message ~translation_target_lang) ~body_type:(get_camel_type emoji)
   ]
    
let () = Command_unix.run ~version:"0.1" camelsay_command