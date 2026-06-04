let executeRequest ~command ~arguments =
  let command = Jsonoo.Encode.string command in
  let arguments = Jsonoo.Encode.list Fun.id arguments in
  Jsonoo.Encode.object_ [ ("command", command); ("arguments", arguments) ]

let move direction server ~args:_ =
  let open Vscode in
  match Window.activeTextEditor () with
  | None -> Ojs.unit_to_js ()
  | Some editor ->
      let document = TextEditor.document editor in
      let uri = TextDocument.uri document in
      let request =
        let arguments = [ Uri.toString uri () |> Jsonoo.Encode.string ] in
        let command =
          match direction with
          | `Forward -> "slipshow.go_next"
          | `Backward -> "slipshow.go_previous"
        in
        executeRequest ~command ~arguments
      in
      let _response =
        Vscode_languageclient.LanguageClient.sendRequest server
          ~meth:"workspace/executeCommand" ~data:request ()
      in
      Ojs.unit_to_js ()

let next = move `Forward
let previous = move `Backward

let register server context =
  let next_disposable =
    Vscode.Commands.registerCommand ~command:"slipshow.next"
      ~callback:(next server)
  in
  Vscode.ExtensionContext.subscribe context ~disposable:next_disposable;
  let previous_disposable =
    Vscode.Commands.registerCommand ~command:"slipshow.previous"
      ~callback:(previous server)
  in
  Vscode.ExtensionContext.subscribe context ~disposable:previous_disposable
