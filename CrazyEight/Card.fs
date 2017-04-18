module Card
    type Rank = 
        | Ace | Two | Three | Four | Five | Six | Seven
        | Eight | Nine | Ten | Jack | Queen | King

    type Suit = Diamond | Heart | Spade | Club

    let ranks = [Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King]
    let suits = [Diamond; Heart; Spade; Club]

    type Card = {Face : Rank; Suit : Suit}

    type Deck = Deck of Card list
    type Hand = Hand of Card list

    type Player = {Name : string; Hand : Hand}

    let createDeck () : Deck =
        let rec createDeckForSuit (suit : Suit) (ranks : Rank list) (acc : Card list) : Card list =
            match ranks with
            | [] -> acc
            | hd :: tl ->
                createDeckForSuit suit tl ({Face=hd; Suit = suit}::acc)

        let rec createDeckForSuits (suits : Suit list) (acc : Card list) : Card list =
            match suits with
            | [] -> acc
            | hd :: tl -> createDeckForSuits tl ((createDeckForSuit hd ranks []) @ acc)
        Deck (createDeckForSuits suits [])

    let createMegaDeck (numberOfDecks : int) : Deck =
        let rec createDecks (numberOfDecks : int) (acc : Card list) : Deck =
            match numberOfDecks with
            | 0 -> Deck acc
            | _ ->
                let newDeck = createDeck ()
                match newDeck with
                | Deck d -> createDecks (numberOfDecks-1) (d @ acc)
        createDecks numberOfDecks []

    let shuffle (deck : Deck) : Deck =
        let random = System.Random()
        match deck with
        | Deck d ->
            Deck (d |> List.sortBy (fun value -> random.Next()))

    let deal (deck : Deck) : (Card * Deck) option =
        match deck with
        | Deck d ->
            match d with
            | [] -> None
            | hd::tl -> (hd, Deck tl) |> Some

    let numberOfCardsInDeck (deck : Deck) : int =
        match deck with
        | Deck d -> d |> List.length