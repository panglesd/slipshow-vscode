let read_file document : Slipshow.file_reader =
 fun s ->
  try
    let s =
      if Fpath.is_abs s then s
      else
        let open Vscode in
        let root = TextDocument.fileName document |> Fpath.v |> Fpath.parent in
        Fpath.( // ) root s
    in
    let content =
      let content =
        Node.Fs.readFileSync @@ Fpath.to_string s
        |> Node.Buffer.toBase64 |> Base64.decode_exn
      in
      content
    in
    Ok (Some content)
  with _ -> Ok None

let slipshow_callback ~args:_ =
  let open Vscode in
  match Window.activeTextEditor () with
  | None -> ()
  | Some editor ->
      let document = TextEditor.document editor in
      let text = TextDocument.getText document () in
      let path =
        TextDocument.fileName document
        |> Fpath.v |> Fpath.set_ext "html" |> Fpath.to_string
      in
      let read_file = read_file document in
      let _ =
        let open Promise.Syntax in
        let+ slipshow_content =
          (* Effect.Deep.try_with *)
          (*   (fun () ->  *)
          Promise.return @@ Slipshow.convert ~read_file text
          (* ) *)
          (* () *)
          (* { *)
          (*   effc = *)
          (*     (fun (type a) (eff : a Effect.t) -> *)
          (*       match eff with *)
          (*       | GetImg path -> *)
          (*           Some *)
          (*             (fun (k : (a, _) Effect.Deep.continuation) -> *)
          (*               let* content = Node.Fs.readFile path in *)
          (*               Effect.Deep.continue k *)
          (*                 (Some *)
          (*                    content *)
          (*                    (\* Slipshow.Remote ("YOOOO" ^ Fpath.to_string path) *\))) *)
          (*       | _ -> None); *)
          (* } *)
        in
        let _message =
          let _ = Node.Fs.writeFile path ~content:slipshow_content in
          ()
          (* match Bos.OS.File.write path slipshow_content with *)
          (* | Error (`Msg m) -> Format.sprintf "Error: %s" m *)
          (* | Ok () -> "OK" *)
        in
        (* let _ = Vscode.Window.showInformationMessage ~message () in *)
        ()
      in
      ()

let preview_callback extension ~args:_ =
  let open Vscode in
  match Window.activeTextEditor () with
  | None -> ()
  | Some editor ->
      let panel =
        Window.createWebviewPanel ~viewType:"slipshowPanel"
          ~title:"Slipshow preview" ~showOptions:ViewColumn.Two
      in
      let document = TextEditor.document editor in
      let the_uri = TextDocument.uri document in
      let text = TextDocument.getText document () in
      let wb = WebviewPanel.webview panel in
      let update_content text =
        let read_file = read_file document in
        let delayed = Slipshow.delayed ~read_file text in
        let text = Slipshow.delayed_to_string delayed in
        WebView.postMessage (WebviewPanel.webview panel) (Ojs.string_to_js text)
      in

      let listener td =
        let document = TextDocumentChangeEvent.document td in
        let uri = TextDocument.uri document in
        let is_good = Uri.equal uri the_uri in
        let document = TextEditor.document editor in
        let text = TextDocument.getText document () in
        let _ = if is_good then update_content text else Promise.return false in
        ()
      in
      let disposable = Workspace.onDidChangeTextDocument ~listener () in
      Vscode.ExtensionContext.subscribe extension ~disposable;
      let options = WebviewOptions.create ~enableScripts:true () in
      let () = WebView.set_options wb options in
      let html =
        Format.sprintf
          {|<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
           <title>Cat Coding</title>
           <style>
           #right-panel1.active_panel, #right-panel2.active_panel {
             z-index: 1;
           }
           #right-panel1, #right-panel2 {
             z-index: 0;
           }
</style>
</head>
           <body>
           <div id="iframes">
	     <iframe name="frame" id="right-panel1" style="width:100%%; position:absolute; top:0;bottom:0;left:0;right:0;border:0; height: 100vh"></iframe>
	     <iframe name="frame" id="right-panel2" style="width:100%%; position:absolute; top:0;bottom:0;left:0;right:0;border:0; height: 100vh"></iframe>
           </div>
           <script>%s</script>
</body>
</html>
         |}
          (* Assets.(read Index_js) *)
          [%blob "../src-panel/vscode_previewer.bc.js"]
      in
      let _ = WebView.set_html wb html in
      let _ = update_content text in
      ()

let activate context =
  let ss_disposable =
    Vscode.Commands.registerCommand ~command:"slipshow.slipshow"
      ~callback:slipshow_callback
  in
  let prs_disposable =
    Vscode.Commands.registerCommand ~command:"slipshow.preview"
      ~callback:(preview_callback context)
  in
  Vscode.ExtensionContext.subscribe context ~disposable:ss_disposable;
  Vscode.ExtensionContext.subscribe context ~disposable:prs_disposable

let () =
  let open Js_of_ocaml.Js in
  export "activate" (wrap_callback activate)
