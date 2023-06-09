open Grace.Diagnostic

module Style = struct
  module Style = struct
    type t = Fmt.style list
  end

  type t =
    { header_bug : Style.t
    ; header_error : Style.t
    ; header_warning : Style.t
    ; header_note : Style.t
    ; header_help : Style.t
    ; header_message : Style.t
    ; primary_label_bug : Style.t
    ; primary_label_error : Style.t
    ; primary_label_warning : Style.t
    ; primary_label_note : Style.t
    ; primary_label_help : Style.t
    ; secondary_label : Style.t
    ; line_number : Style.t
    ; source_border : Style.t
    ; note_bullet : Style.t
    }

  let default =
    let header color = [ `Bold; `Fg (`Hi color) ] in
    let primary_label color = [ `Fg color ] in
    { header_bug = header `Red
    ; header_error = header `Red
    ; header_warning = header `Yellow
    ; header_note = header `Green
    ; header_help = header `Cyan
    ; header_message = [ `Bold ]
    ; primary_label_bug = primary_label `Red
    ; primary_label_error = primary_label `Red
    ; primary_label_warning = primary_label `Yellow
    ; primary_label_note = primary_label `Green
    ; primary_label_help = primary_label `Cyan
    ; secondary_label = [ `Fg `Cyan ]
    ; line_number = [ `Fg `Cyan ]
    ; source_border = [ `Fg `Cyan ]
    ; note_bullet = [ `Fg `Cyan ]
    }
  ;;

  let header config (severity : Severity.t) =
    match severity with
    | Bug -> config.header_bug
    | Error -> config.header_error
    | Warning -> config.header_warning
    | Note -> config.header_note
    | Help -> config.header_help
  ;;

  let label config (priority : Priority.t) (severity : Severity.t) =
    match priority, severity with
    | Primary, Bug -> config.primary_label_bug
    | Primary, Error -> config.primary_label_error
    | Primary, Warning -> config.primary_label_warning
    | Primary, Note -> config.primary_label_note
    | Primary, Help -> config.primary_label_help
    | Secondary, _ -> config.secondary_label
  ;;
end

module Chars = struct
  type t =
    { snippet_start : string
    ; source_border_left : string
    ; source_border_left_break : string
    ; note_bullet : string
    ; single_primary_caret : string
    ; single_secondary_caret : string
    ; multi_primary_caret_start : string
    ; multi_primary_caret_end : string
    ; multi_secondary_caret_start : string
    ; multi_secondary_caret_end : string
    ; multi_top_left : string
    ; multi_top : string
    ; multi_bottom_left : string
    ; multi_bottom : string
    ; multi_left : string
    ; pointer_left : string
    }

  let unicode =
    { snippet_start = "┌─"
    ; source_border_left = "│"
    ; source_border_left_break = "·"
    ; note_bullet = "="
    ; single_primary_caret = "^"
    ; single_secondary_caret = "-"
    ; multi_primary_caret_start = "^"
    ; multi_primary_caret_end = "^"
    ; multi_secondary_caret_start = "\'"
    ; multi_secondary_caret_end = "\'"
    ; multi_top_left = "╭"
    ; multi_top = "─"
    ; multi_bottom_left = "╰"
    ; multi_bottom = "─"
    ; multi_left = "│"
    ; pointer_left = "│"
    }
  ;;
end

type t =
  { chars : Chars.t
  ; style : Style.t
  ; color : bool
  }

let default = { chars = Chars.unicode; style = Style.default; color = true }
