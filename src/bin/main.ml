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
      let slipshow_content = Slipshow.convert text in
      let _message =
        let _ = Node.Fs.writeFile path ~content:slipshow_content in
        ()
        (* match Bos.OS.File.write path slipshow_content with *)
        (* | Error (`Msg m) -> Format.sprintf "Error: %s" m *)
        (* | Ok () -> "OK" *)
      in
      (* let _ = Vscode.Window.showInformationMessage ~message () in *)
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
      (* let path = *)
      (*   TextDocument.fileName document *)
      (*   |> Fpath.v |> Fpath.set_ext "html" |> Fpath.to_string *)
      (* in *)
      let wb = WebviewPanel.webview panel in

      (* let disposable = *)
      (*   Window.onDidChangeActiveTextEditor () *)
      (*     ~listener:(fun _td -> *)
      (*       let _ = *)
      (*         Vscode.Window.showInformationMessage *)
      (*           ~message:"Changed Text was called" () *)
      (*       in *)
      (*       ()) *)
      (*     () *)
      (* in *)
      let update_content text =
        WebView.postMessage
          (WebviewPanel.webview panel)
          (Ojs.string_to_js (* slipshow_content *) text)
      in

      let listener td =
        let document = TextDocumentChangeEvent.document td in
        let uri = TextDocument.uri document in
        let is_good = Uri.equal uri the_uri in
        let document = TextEditor.document editor in
        let text = TextDocument.getText document () in
        (* let slipshow_content = Slipshow.convert text in *)
        (* let _ = WebView.set_html wb html in *)
        let _ = if is_good then update_content text else Promise.return false in
        ()
      in
      let disposable = Workspace.onDidChangeTextDocument ~listener () in
      Vscode.ExtensionContext.subscribe extension ~disposable;
      let _slipshow_content = Slipshow.convert text in
      (* let _message = *)
      (*   let _ = Node.Fs.writeFile path ~content:slipshow_content in *)
      (*   () *)
      (*   (\* match Bos.OS.File.write path slipshow_content with *\) *)
      (*   (\* | Error (`Msg m) -> Format.sprintf "Error: %s" m *\) *)
      (*   (\* | Ok () -> "OK" *\) *)
      (* in *)
      (* (\* let _ = Vscode.Window.showInformationMessage ~message () in *\) *)
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
          [%blob "src/src-panel/vscode_previewer.bc.js"]
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
