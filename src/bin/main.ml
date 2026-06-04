let client_state = ref None

let activate context =
  let open Vscode_languageclient in
  let serverOptions =
    ServerOptions.create ~args:[ "lsp" ] ~command:"slipshow" ()
  in
  let documentSelector =
    [|
      DocumentSelector.language ~scheme:"file" ~pattern:"**/*.slp" "slipshow";
    |]
  in
  let clientOptions = ClientOptions.create ~documentSelector () in
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
