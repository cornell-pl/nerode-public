(** Parser for regular expressions over the alphabet ["0"; "1"] *)

include Nice_parser.Make(struct
  type result = Rx.t
  type token = Rx_parser.token
  exception ParseError = Rx_parser.Error
  let parse = Rx_parser.rx_eof
  include Rx_lexer
end)
