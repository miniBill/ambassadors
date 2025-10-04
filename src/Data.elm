module Data exposing (Country(..), Player(..), countries, initialCountry, playerToString, players)

import Area exposing (Area)


type Player
    = Alex
    | Emily
    | Leonardo
    | Liv
    | Soph
    | Wouter


players : List Player
players =
    [ Alex
    , Emily
    , Leonardo
    , Liv
    , Soph
    , Wouter
    ]


playerToString : Player -> String
playerToString player =
    case player of
        Alex ->
            "Alex"

        Emily ->
            "Ems"

        Leonardo ->
            "Leo"

        Liv ->
            "Liv"

        Soph ->
            "Soph"

        Wouter ->
            "Woot"


initialCountry : Player -> Country
initialCountry player =
    case player of
        Leonardo ->
            SanMarino

        Liv ->
            Andorra

        Soph ->
            Monaco

        Wouter ->
            Liechtenstein

        Alex ->
            Luxembourg

        Emily ->
            Netherlands


type Country
    = Albania
    | Andorra
    | Austria
    | Belarus
    | Belgium
    | BosniaAndHerzegovina
    | Bulgaria
    | Croatia
    | Cyprus
    | CzechRepublic
    | Denmark
    | Estonia
    | Finland
    | France
    | Germany
    | Greece
    | Hungary
    | Iceland
    | Ireland
    | Italy
    | Kosovo
    | Latvia
    | Liechtenstein
    | Lithuania
    | Luxembourg
    | Malta
    | Moldova
    | Monaco
    | Montenegro
    | Netherlands
    | NorthMacedonia
    | Norway
    | Poland
    | Portugal
    | Romania
    | SanMarino
    | Serbia
    | Slovakia
    | Slovenia
    | Spain
    | Sweden
    | Switzerland
    | Ukraine
    | UnitedKingdom
    | Vatican


countries : List Country
countries =
    [ Albania
    , Andorra
    , Austria
    , Belarus
    , Belgium
    , BosniaAndHerzegovina
    , Bulgaria
    , Croatia
    , Cyprus
    , CzechRepublic
    , Denmark
    , Estonia
    , Finland
    , France
    , Germany
    , Greece
    , Hungary
    , Iceland
    , Ireland
    , Italy
    , Kosovo
    , Latvia
    , Liechtenstein
    , Lithuania
    , Luxembourg
    , Malta
    , Moldova
    , Monaco
    , Montenegro
    , Netherlands
    , NorthMacedonia
    , Norway
    , Poland
    , Portugal
    , Romania
    , SanMarino
    , Serbia
    , Slovakia
    , Slovenia
    , Spain
    , Sweden
    , Switzerland
    , Ukraine
    , UnitedKingdom
    , Vatican
    ]


countryArea : Country -> Area
countryArea country =
    case country of
        Albania ->
            Area.squareKilometers 28748

        Andorra ->
            Area.squareKilometers 468

        Austria ->
            Area.squareKilometers 83879

        Belarus ->
            Area.squareKilometers 207595

        Belgium ->
            Area.squareKilometers 32545

        BosniaAndHerzegovina ->
            Area.squareKilometers 51129

        Bulgaria ->
            Area.squareKilometers 110994

        Croatia ->
            Area.squareKilometers 56542

        Cyprus ->
            Area.squareKilometers 9251

        CzechRepublic ->
            Area.squareKilometers 78866

        Denmark ->
            Area.squareKilometers 43098

        Estonia ->
            Area.squareKilometers 45227

        Finland ->
            Area.squareKilometers 338144

        France ->
            Area.squareKilometers 543965

        Germany ->
            Area.squareKilometers 357121

        Greece ->
            Area.squareKilometers 131957

        Hungary ->
            Area.squareKilometers 93030

        Iceland ->
            Area.squareKilometers 103000

        Ireland ->
            Area.squareKilometers 70273

        Italy ->
            Area.squareKilometers 301336

        Kosovo ->
            Area.squareKilometers 10887

        Latvia ->
            Area.squareKilometers 64589

        Liechtenstein ->
            Area.squareKilometers 160

        Lithuania ->
            Area.squareKilometers 65301

        Luxembourg ->
            Area.squareKilometers 2586

        Malta ->
            Area.squareKilometers 316

        Moldova ->
            Area.squareKilometers 33800

        Monaco ->
            Area.squareKilometers 2

        Montenegro ->
            Area.squareKilometers 13812

        Netherlands ->
            Area.squareKilometers 41526

        NorthMacedonia ->
            Area.squareKilometers 25713

        Norway ->
            Area.squareKilometers 323759

        Poland ->
            Area.squareKilometers 312685

        Portugal ->
            Area.squareKilometers 92345

        Romania ->
            Area.squareKilometers 238397

        SanMarino ->
            Area.squareKilometers 61

        Serbia ->
            Area.squareKilometers 77474

        Slovakia ->
            Area.squareKilometers 49034

        Slovenia ->
            Area.squareKilometers 20253

        Spain ->
            Area.squareKilometers 504645

        Sweden ->
            Area.squareKilometers 449964

        Switzerland ->
            Area.squareKilometers 41285

        Ukraine ->
            Area.squareKilometers 603700

        UnitedKingdom ->
            Area.squareKilometers 242910

        Vatican ->
            Area.squareKilometers 0.44


countryToPopulation : Country -> Int
countryToPopulation country =
    case country of
        Germany ->
            84075074

        UnitedKingdom ->
            69551332

        France ->
            66650804

        Italy ->
            59146260

        Spain ->
            47889958

        Ukraine ->
            38980376

        Poland ->
            38140910

        Romania ->
            18908650

        Netherlands ->
            18346819

        Belgium ->
            11758603

        Sweden ->
            10656633

        CzechRepublic ->
            10609240

        Portugal ->
            10411834

        Greece ->
            9938844

        Hungary ->
            9632287

        Austria ->
            9113574

        Belarus ->
            8997603

        Switzerland ->
            8967408

        Bulgaria ->
            6714560

        Serbia ->
            6689039

        Denmark ->
            6002507

        Finland ->
            5623330

        Norway ->
            5623071

        Slovakia ->
            5474881

        Ireland ->
            5308039

        Croatia ->
            3848160

        BosniaAndHerzegovina ->
            3140096

        Moldova ->
            2996106

        Lithuania ->
            2830144

        Albania ->
            2771508

        Slovenia ->
            2117072

        Latvia ->
            1853559

        NorthMacedonia ->
            1813791

        Kosovo ->
            1674125

        Cyprus ->
            1370754

        Estonia ->
            1344232

        Luxembourg ->
            680454

        Montenegro ->
            632729

        Malta ->
            545405

        Iceland ->
            398266

        Andorra ->
            82904

        Liechtenstein ->
            40128

        Monaco ->
            38341

        SanMarino ->
            33572

        Vatican ->
            764
