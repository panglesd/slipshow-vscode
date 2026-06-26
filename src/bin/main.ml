let client_state = ref None

let activate context =
  let open Vscode_languageclient in
  let serverOptions =
    ServerOptions.create ~args:[ "lsp" ] ~command:"slipshow" ()
  in
  let documentSelector =
    [|
      DocumentSelector.language ~scheme:"file" ~pattern:"**/*.slp" "markdown";
    |]
  in
  let clientOptions = ClientOptions.create ~documentSelector () in
  (* We don't need this anymore, as we are using the pull model correctly now *)
  (* let clientOptions = *)
  (*   let ojs = clientOptions |> ClientOptions.t_to_js in *)
  (*   (\* See *)
  (*      https://github.com/microsoft/vscode-languageserver-node/blob/main/client/src/common/configuration.ts#L145 *)
  (*      for the synchronize option "documentation" *\) *)
  (*   let synchronize_options = *)
  (*     let res = Ojs.empty_obj () in *)
  (*     Ojs.set_prop_ascii res "configurationSection" *)
  (*       (Ojs.string_to_js "slipshow"); *)
  (*     res *)
  (*   in *)
  (*   Ojs.set_prop_ascii ojs "synchronize" synchronize_options; *)
  (*   ojs |> ClientOptions.t_of_js *)
  (* in *)
  let id = "slipshow" and name = "Slipshow" in
  let client = LanguageClient.make ~id ~name ~serverOptions ~clientOptions () in
  client_state := Some client;
  let open Promise.Syntax in
  let+ () = LanguageClient.start client in
  Control.register client context

let deactivate _ =
  match !client_state with
  | Some client -> Vscode_languageclient.LanguageClient.stop client
  | None -> Promise.return ()

let () =
  let open Js_of_ocaml.Js in
  export "activate" (wrap_callback activate)

let () =
  let open Js_of_ocaml.Js in
  export "deactivate" (wrap_callback deactivate)
