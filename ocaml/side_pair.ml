open Core.Std

type 'a t = { top: 'a; bot: 'a } with sexp

let get t side = 
  match (side : Side.t) with Top -> t.top | Bot -> t.bot

let set t side x =
  match (side : Side.t) with
  | Top -> { t with top = x }
  | Bot -> { t with bot = x }

let of_fn (f : Side.t -> 'a) =
  { top = f Top
  ; bot = f Bot
  }
 
let map t ~f =
  { top = f t.top
  ; bot = f t.bot
  }

let change t side ~f =
  set t side (f (get t side))
