(* Define types for orders *)
type order_type = Buy | Sell

type order = {
  id: int;
  order_type: order_type;
  price: float;
  quantity: int;
}

(* An order book is a list of orders *)
type order_book = {
  mutable buy_orders: order list;
  mutable sell_orders: order list;
}

(* Initialize an empty order book *)
let init_order_book () = { buy_orders = []; sell_orders = [] }

(* Function to add an order to the order book *)
let add_order book order =
  match order.order_type with
  | Buy ->
      book.buy_orders <- List.sort
        (fun o1 o2 ->
          if o1.price = o2.price then compare o1.id o2.id
          else compare o2.price o1.price)
        (order :: book.buy_orders)
  | Sell ->
      book.sell_orders <- List.sort
        (fun o1 o2 ->
          if o1.price = o2.price then compare o1.id o2.id
          else compare o1.price o2.price)
        (order :: book.sell_orders)

(* Function to match buy and sell orders *)
let match_orders book =
  let rec process_matches () =
    match (book.buy_orders, book.sell_orders) with
    | (buy :: buys, sell :: sells) when buy.price >= sell.price ->
        let quantity_traded = min buy.quantity sell.quantity in
        Printf.printf
          "Trade executed: Buy order %d matched with Sell order %d. Quantity: \
          %d\n"
          buy.id sell.id quantity_traded;
        let updated_buy = { buy with quantity = buy.quantity - quantity_traded } in
        let updated_sell = { sell with quantity = sell.quantity - quantity_traded } in
        book.buy_orders <- (if updated_buy.quantity > 0 then [ updated_buy ] else []) @ buys;
        book.sell_orders <- (if updated_sell.quantity > 0 then [ updated_sell ] else []) @ sells;
        process_matches ()
    | _ -> ()
  in
  process_matches ()

(* Test the order book with some sample orders *)
let () =
  let book = init_order_book () in

  (* Add some orders *)
  add_order book { id = 1; order_type = Buy; price = 100.0; quantity = 10 };
  add_order book { id = 2; order_type = Sell; price = 99.0; quantity = 5 };
  add_order book { id = 3; order_type = Buy; price = 101.0; quantity = 20 };
  add_order book { id = 4; order_type = Sell; price = 101.0; quantity = 15 };

  (* Match orders *)
  match_orders book;

  (* Print remaining orders *)
  Printf.printf "Remaining Buy Orders:\n";
  List.iter
    (fun o ->
      Printf.printf "ID: %d, Price: %.2f, Quantity: %d\n" o.id o.price o.quantity)
    book.buy_orders;

  Printf.printf "Remaining Sell Orders:\n";
  List.iter
    (fun o ->
      Printf.printf "ID: %d, Price: %.2f, Quantity: %d\n" o.id o.price o.quantity)
    book.sell_orders;
