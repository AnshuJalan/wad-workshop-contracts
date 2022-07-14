#include "./lottery_1.mligo"

let test =
    // setup
    let _ = Test.reset_state 4n ([]: tez list) in
    let player1 = Test.nth_bootstrap_account 1 in
    let player2 = Test.nth_bootstrap_account 2 in
    let _ = Test.set_source (player1) in
    let tickets = 2n in
    // lottery contract origination
    let initial_storage =
        {
            players             = (Map.empty: (nat, address) map);
            ticket_cost         = 1tez;
            tickets_available   = tickets;
            max_tickets         = tickets;
        }
    in
    let lottery_addr, _, _ = 
        Test.originate_from_file 
            "./lottery_1.mligo" 
            "main"
            ([]: string list)
            (Test.compile_value initial_storage) 
            0tez 
    in
    // checks the contract has been originated properly
    let lottery_typed_addr: (param, storage) typed_address = Test.cast_address lottery_addr in
    let storage = Test.get_storage lottery_typed_addr in
    let _ = assert (storage.max_tickets = tickets) in

    let lottery_contract = Test.to_contract lottery_typed_addr in
    // buy_ticket should fail with wrong amount
    let _ = 
        match (Test.transfer_to_contract lottery_contract Buy_ticket 0tez) with
        | Success _ -> assert false
        | Fail err -> (
            match err with
            | Rejected (err, _) ->
                if err = (Test.compile_value "INVALID_AMOUNT")
                then assert true
                else assert false
            | _ -> assert false
        )
    in

    // testing the buy_ticket entrypoint
    let _ = 
        match (Test.transfer_to_contract lottery_contract Buy_ticket storage.ticket_cost) with
        | Success _ -> assert true
        | Fail err -> (
            let _ = Test.log err in
            assert false
        )
    in

    // buys the last ticket
    let _ = Test.set_source (player2) in
    let _ = 
        match (Test.transfer_to_contract lottery_contract Buy_ticket storage.ticket_cost) with
        | Success _ -> assert true
        | Fail err -> (
            let _ = Test.log err in
            assert false
        )
    in

    let storage = Test.get_storage lottery_typed_addr in
    let _ = assert (storage.tickets_available = 0n) in

    // buy_ticket should fail now
    let _ = 
        match (Test.transfer_to_contract lottery_contract Buy_ticket storage.ticket_cost) with
        | Success _ -> assert false
        | Fail err -> (
            match err with
            | Rejected (err, _) ->
                if err = (Test.compile_value "NO_TICKETS_AVAILABLE")
                then assert true
                else assert false
            | _ -> assert false
        )
    in

    // end_game
    let player1_initial_balance = Test.get_balance player1 in
    let player2_initial_balance = Test.get_balance player2 in
    let _ = 
        match (Test.transfer_to_contract lottery_contract End_game 0tez) with
        | Success _ -> assert true
        | Fail _ -> assert false
    in
    let storage = Test.get_storage lottery_typed_addr in
    let _ = assert (Map.size storage.players = 0n) in
    let _ = assert (storage.tickets_available = 2n) in

    let player1_new_balance = Test.get_balance player1 in
    let player2_new_balance = Test.get_balance player2 in
    let _ = 
        assert (
            player1_new_balance > player1_initial_balance || player2_new_balance > player2_initial_balance
        ) 
    in

    ()