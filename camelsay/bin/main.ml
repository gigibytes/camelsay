open Core

let render_output ~message ~body_type =
  (* TODO idk how to get unicode actually rendering so this is here as a regular string for now. doubt this works *)
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


(* this feels a little redundant but I guess I can justify it by saying that the
  render function is just using these types to decide what to render, instead of having to
  match directly on the bool? maybe it's a little more intentional/separated? *)
let get_camel_type emoji_flag = if emoji_flag then `Emoji else `Ascii;;

let get_translation ~message ~language_code = 
 if (not (String.equal language_code "no_op")) then
  (* TODO translate 4real *)
  (* let api_translation = ... *)
  (* https://cloud.google.com/translate/docs/reference/rpc/google.cloud.translation.v3beta1 *)
    message ^ " " ^ [%string "this message has been translated to %{language_code}"]
  else message ^ "No translation performed. Language not recognized"
;;

let translate ~message ~language =
  let language_code =
    (* About language codes
    https://en.wikipedia.org/wiki/IETF_language_tag
    https://www.techonthenet.com/js/language_tags.php
    https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes
    *)
    (* TODO: support non roman chars and accents with unicode *)
    (* If we are in this codepath then ~language shouldn't be None. *)
    (* This language validation should probly either happen by checking valid langs against api, or just 
      matching on api error if we pass a bad lang code *)
    match language with
    | Some l when String.Caseless.equal l "es" -> "es_SP"
    | _ -> "no_op"
  in
  if (not (String.equal language_code "no_op")) then
    get_translation ~message ~language_code
  else message
;;

let get_message ~message ~lang = 
  let default_message = "*spits*"
  in
  match message with
  | Some m when (Option.is_some lang) -> translate ~message:m ~language:lang
  (* Don't translate when the translation_target_lang option is None/when guard eq false *)
  | Some m -> m
  | None -> default_message
;;

let camelsay_command =
  (* Command.basic lets you build a Command with Param types instead of Spec *)
  Command.basic
  ~summary:"Forces the camel to utter a phrase of your choosing."
  [%map_open.Command
  (* Command.Param below *)
    (* this anon function is part of Command.Param *)
    let message = anon (maybe ("message" %: string))
    and
    (* Command.Params can take Command.Flag types in their definitions. https://ocaml.org/p/core/v0.12.3/doc/Core/Command/Param/index.html#val-flag *)
    emoji = flag "emoji" Command.Flag.no_arg ~doc:"Display an emoji camel instead of the default ASCII camel"
    and
    (* Should offer a set of options to start *)
    lang = flag "lang" (optional string) ~doc:"Use Google translate to output a different language"
    in 
    fun () -> render_output ~message:(get_message ~message ~lang) ~body_type:(get_camel_type emoji)
   ]
    
let () = Command_unix.run ~version:"0.1" camelsay_command