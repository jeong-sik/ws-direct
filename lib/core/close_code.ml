type t = int

let normal = 1000
let protocol_error = 1002
let invalid_payload = 1007
let message_too_big = 1009

(* RFC 6455 §7.4.1 registry. Codes valid to RECEIVE on the wire are the
   registered protocol codes 1000-1003 and 1007-1011, plus the application range
   3000-4999. Everything else is rejected: 0-999 are out of the close-code
   space, 1004/1005/1006/1015 are reserved and MUST NOT appear on the wire,
   1012-2999 are unassigned/reserved, and >=5000 is undefined. *)
let is_receivable n =
  (n >= 1000 && n <= 1003) || (n >= 1007 && n <= 1011) || (n >= 3000 && n <= 4999)

let of_wire n =
  if is_receivable n then Ok n
  else Error (Printf.sprintf "invalid close code %d" n)

let to_int t = t
