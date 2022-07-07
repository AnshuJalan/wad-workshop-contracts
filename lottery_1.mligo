type param =
    | Buy_ticket
    | End_game

type storage =
{
    players             : (nat, address) map;
    ticket_cost         : tez;
    tickets_available   : nat;
    max_tickets         : nat;
}

type return = operation list * storage

let buy_ticket (s: storage): return =
    let sender = Tezos.get_sender () in
    // checks if tickets are available
    if s.tickets_available = 0n
    then failwith "NO_TICKETS_AVAILABLE"
    // checks if the right amount was sent
    else if (Tezos.get_amount ()) < s.ticket_cost
    then failwith "INVALID_AMOUNT"
    else
        // updates the players map
        let new_players = Map.add (Map.size s.players) sender s.players in
        // updates the number of tickets
        let new_tickets = abs(s.tickets_available - 1n) in
        // returns extra tez balance to the sender
        let amount_to_return =
            match s.ticket_cost - (Tezos.get_amount ()) with
            | None -> failwith "SUB_MUTEZ_OVERFLOW"
            | Some r -> r
        in
        let ops = 
            if amount_to_return > 0tez
            then
                let sender_contract = match ((Tezos.get_contract_opt sender): unit contract option) with
                    | None -> failwith "CANNOT_GET_SENDER"
                    | Some sender -> sender
                in [Tezos.transaction () amount_to_return sender_contract]
            else
                []
        in

        ops,
        {
            s with
                players = new_players;
                tickets_available = new_tickets;
        }

let end_game (s: storage): return =
    // checks if all tickets have been sold
    if s.tickets_available <> 0n
    then failwith "GAME_IS_YET_TO_END"
    else
        // picks a winner
        let winner_id = abs((Tezos.get_now ()) - (0: timestamp)) mod s.max_tickets in
        let winner_address =
            match Map.find_opt winner_id s.players with
            | None -> failwith "WINNER_ID_DOESNT_EXIST"
            | Some w -> w
        in
        // sends the reward to the winner
        let op = 
            let winner_contract = match ((Tezos.get_contract_opt winner_address): unit contract option) with
                | None -> failwith "CANNOT_GET_WINNER"
                | Some w -> w
            in Tezos.transaction () (Tezos.get_balance ()) winner_contract
        in

        // resets the game
        [op],
        {
            s with
                players = (Map.empty: (nat, address) map);
                tickets_available = s.max_tickets;
        }

let main (p, s: param * storage): return =
    match p with
    | Buy_ticket -> buy_ticket s
    | End_game -> end_game s