(* Incremental UTF-8 validator following the well-formed byte-sequence table of
   the Unicode standard (RFC 3629 §4 / Unicode Table 3-7).

   State carries the number of continuation bytes still expected and the smallest
   code point the current lead byte may legally produce ([min_cp]); checking the
   decoded value against [min_cp] rejects overlong encodings. Surrogates
   (U+D800..U+DFFF) and code points above U+10FFFF are rejected explicitly. The
   bit/range constants below are intrinsic to UTF-8, not tunable. *)

type t =
  { mutable valid : bool
  ; mutable need : int (* continuation bytes still expected for the current cp *)
  ; mutable cp : int (* code point accumulated so far *)
  ; mutable min_cp : int (* smallest legal code point for the current lead byte *)
  }

let create () = { valid = true; need = 0; cp = 0; min_cp = 0 }

let fail t =
  t.valid <- false;
  false

let byte t b =
  if not t.valid then false
  else if t.need = 0 then
    if b < 0x80 then true (* ASCII *)
    else if b < 0xC2 then fail t
      (* 0x80-0xBF: stray continuation; 0xC0/0xC1: always overlong leads *)
    else if b < 0xE0 then begin
      t.need <- 1;
      t.cp <- b land 0x1F;
      t.min_cp <- 0x80;
      true
    end
    else if b < 0xF0 then begin
      t.need <- 2;
      t.cp <- b land 0x0F;
      t.min_cp <- 0x800;
      true
    end
    else if b < 0xF5 then begin
      t.need <- 3;
      t.cp <- b land 0x07;
      t.min_cp <- 0x10000;
      true
    end
    else fail t (* 0xF5-0xFF: would exceed U+10FFFF *)
  else if b < 0x80 || b >= 0xC0 then fail t (* expected a continuation byte *)
  else begin
    t.cp <- (t.cp lsl 6) lor (b land 0x3F);
    t.need <- t.need - 1;
    if t.need > 0 then true
    else if t.cp < t.min_cp then fail t (* overlong encoding *)
    else if t.cp >= 0xD800 && t.cp <= 0xDFFF then fail t (* surrogate half *)
    else if t.cp > 0x10FFFF then fail t (* beyond Unicode range *)
    else begin
      t.cp <- 0;
      t.min_cp <- 0;
      true
    end
  end

let feed t bs ~off ~len =
  let stop = off + len in
  let i = ref off in
  while t.valid && !i < stop do
    ignore (byte t (Char.code (Bigstringaf.unsafe_get bs !i)));
    incr i
  done;
  t.valid

let feed_string t s =
  let n = String.length s in
  let i = ref 0 in
  while t.valid && !i < n do
    ignore (byte t (Char.code (String.unsafe_get s !i)));
    incr i
  done;
  t.valid

let is_complete t = t.valid && t.need = 0

let valid_string s =
  let t = create () in
  let _ = feed_string t s in
  is_complete t

let valid_bigstring bs ~off ~len =
  let t = create () in
  let _ = feed t bs ~off ~len in
  is_complete t
