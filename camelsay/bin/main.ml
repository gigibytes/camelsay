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
(* this is where we reach out to whatever API i'm using for this, probably Google Translate
https://cloud.google.com/translate/docs/reference/rpc/google.cloud.translation.v3beta1
*)
 if (not (String.equal language_code "no_op")) then
    message ^ "this message has been translated :)"
  else message
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
    match language with
    | Some l when String.Caseless.equal l "es" -> "es_SP"
    | _ -> "no_op"
  in
  let translation_note =
    match language_code with
    (* If we are in this codepath then ~language shouldn't be None,
      it would be a string that didn't match any of the options in the list *)
    (* TODO: relocate validation to the Command definition. Check if string is one of the API supported options.
      Can probably switch to using a bigass variant to switch on language type *)
    | "no_op" -> "No translation performed. Language not recognized"
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
    translation_target_lang = flag "translate" (optional string) ~doc:"Use Google translate to output a different language"
    in 
    fun () -> render_output ~message:(get_message ~message ~translation_target_lang) ~body_type:(get_camel_type emoji)
   ]
    
let () = Command_unix.run ~version:"0.1" camelsay_command