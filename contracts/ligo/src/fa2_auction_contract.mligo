type owner = address

type auction_params = {
  auction_owner: owner;
  master_auction_contract: address;
  asset_id: nat;
  current_bid: tez;
  min_increase: tez;
  highest_bidder: address;
  started: bool;
  first_bid_placed: bool;
  ended: bool;
  start_time: timestamp;
  round_time: int;
  oracle: address;
  token: address;
  next_request_id: nat;
  waiting_request_id: nat option;
  oracle_job_id: bytes;
}

type auction_storage = auction_params

type client_request_id = int

type result = int

type asset_id = int

type auction_entrypoints =
       ConfigureAuction of (tez * auction_params)
     | StartAuction of auction_params
     | Bid of auction_params
     | Callback of client_request_id * result
     | RequestTrackInfo of auction_params
     | CancelTrackRequest
     | ResolveAuction
     | CancelAuction of auction_params
     | EndAuction of asset_id * owner

let dummy_value (storage : auction_storage)
    : operation list * auction_storage =
      ([] : operation list) , storage

let fail_chain (condition, message: bool * string) : unit =
    if condition then failwith message else unit

let configure_auction (reserve_price : tez) (params, storage : auction_params * auction_storage)
  : (operation list) * auction_storage =
    let sender_fail_msg = "" in
    let sender_validate = fail_chain (Tezos.sender <> storage.auction_owner, sender_fail_msg) in
    let started_fail_msg = "" in
    let started_validate = fail_chain (not storage.started, started_fail_msg) in
    let first_bid_placed_fail_msg = "" in
    let first_bid_placed_validate = fail_chain (not storage.first_bid_placed, first_bid_placed_fail_msg) in
    let ended_msg = "" in
    let ended_validate = fail_chain (not storage.ended, ended_msg) in
    let start_time_fail_msg = "" in
    let start_time_validate = fail_chain(params.start_time > Tezos.now, start_time_fail_msg) in
    let new_storage = {storage with
             current_bid = reserve_price;
             min_increase = params.min_increase;
             start_time = params.start_time;
             round_time = params.round_time;} in
    // change variable name eventually
    // let callAuctionFactory = failwith "" in
    // let t = Operation.transaction
    ([] : operation list), new_storage

let start_auction (params, storage: auction_params * auction_storage) : (operation list) * auction_storage =
    let sender_fail_msg = "" in
    let sender_validate = fail_chain (Tezos.sender <> storage.auction_owner, sender_fail_msg) in
    let started_fail_msg = "" in
    let started_validate = fail_chain (not storage.started, started_fail_msg) in
    let first_bid_placed_fail_msg = "" in
    let first_bid_placed_validate = fail_chain (not storage.first_bid_placed, first_bid_placed_fail_msg) in
    let ended_msg = "" in
    let ended_validate = fail_chain (not storage.ended, ended_msg) in
    let round_end_time_fail_msg = "" in
    let start_time_validate = fail_chain(Tezos.now < storage.start_time + storage.round_time, round_end_time_fail_msg) in
    let new_storage = {storage with
                   started = true;
                   start_time = Tezos.now;
                   } in
    // let callAuctionFactory = failwith "" in
    // let t = some_operation in
    ([] : operation list), new_storage

let bid (params, storage: auction_params * auction_storage) : (operation list) * auction_storage =
    let started_validate = assert(storage.started) in
    let ended_validate = assert(not storage.ended) in
    let gt_min_increase_validate = assert(Tezos.amount - storage.current_bid >= storage.min_increase) in
    let highest_bidder_validate = assert(storage.highest_bidder <> Tezos.sender) in
    let sender_source_validate = assert(Tezos.source = Tezos.sender) in
    let round_end_time_validate = assert(Tezos.now < storage.start_time + storage.round_time) in
    let new_storage = {storage with
        first_bid_placed = if not storage.first_bid_placed then true else storage.first_bid_placed;
        current_bid = Tezos.amount;
        highest_bidder = Tezos.sender;
        start_time = Tezos.now;
        } in
    // // smartpy code sends current bid to highest bidder? I'm not sure why that is done.
    // let ops = if not storage.first_bid_placed then [] else Operation.transaction  in
    ([]: operation list), new_storage

let auction_main (param, storage: auction_entrypoints * auction_storage)
    : operation list * auction_storage =
  match param with
  | ConfigureAuction auction_params -> dummy_value storage
  | StartAuction auction_params -> dummy_value storage
  | Bid auction_params -> dummy_value storage
  | Callback callback_params ->
    let client_request = callback_params.0 in
    let result = callback_params.1
    in dummy_value storage
  | RequestTrackInfo auction_params -> dummy_value storage
  | CancelTrackRequest -> dummy_value storage
  | ResolveAuction -> dummy_value storage
  | CancelAuction auction_params -> dummy_value storage
  | EndAuction end_auction_params ->
    let asset_id = end_auction_params.0
    in let owner = end_auction_params.1
    in dummy_value storage
