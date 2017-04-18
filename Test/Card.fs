module CardTest

open NUnit.Framework

[<Test>]
let ``Given a deck when I deal once then deck reduces in size by one`` () =
    let deck = Card.createDeck ()
    let sizeOfOriginalDeck = deck |> Card.numberOfCardsInDeck
    let result = Card.deal deck
    match result with
    | None -> Assert.Fail("There should be output")
    | Some (card, newDeck) ->
        let sizeOfNewDeck = newDeck |> Card.numberOfCardsInDeck
        Assert.That(sizeOfNewDeck, Is.EqualTo(sizeOfOriginalDeck - 1))

[<Test>]
let ``Given an empty deck when I deal I get nothing back`` () =
    let deck = Card.Deck []
    let result = Card.deal deck
    Assert.That(result, Is.EqualTo(None))
