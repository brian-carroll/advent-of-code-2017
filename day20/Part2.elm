module Part2 exposing (..)

import Part1 exposing (Triplet, Particle, inputParser)
import Input exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)
import Parser


type alias Event =
    ( Int, Int, Int, Int )



{-
   Question is about when ALL collisions finished.
   How do we detect that we're *never* going to have any more?
   Under what conditions can two particles NEVER collide?

   Idea:
        - solve the equations of motion for the collision point
        - if it's imaginary, they don't collide

    Approach:
        Solve for times of 2 potential collisions in X dimension
        Calculate Y and Z at the same tick, see if they also collide

    Derive "equations of motion" from question text
    (Don't use Newton's laws, they're continuous and this is discrete)

    t0: x           v       a
    t1: x+v+a       v+a     a
    t2: x+2v+3a      v+2a    a
    t3: x+3v+6a      v+3a    a
    t4: x+4v+10a     v+4a    a
    t5: x+5v+15a     v+5a    a
    t6: x+6v+21a     v+6a    a
    t7: x+7v+28a     v+7a    a

    Triangular numbers
        0.5*n*(n+1)

    Equations
    p1: x1 + v1*t + 0.5*t*(t+1)*a1
    p2: x2 + v2*t + 0.5*t*(t+1)*a2
    ----------------------------
     0 = x1-x2 + (v1-v2)*t + 0.5*t*(t+1)*(a1-a2)
     0 = x1-x2 + (v1-v2)*t + 0.5*(t^2 + t)*(a1-a2)
     0 = x1-x2 + (v1-v2)*t + 0.5*(a1-a2)*t + 0.5*(a1-a2)*t^2
     0 = x1-x2 + ((v1-v2) + 0.5*(a1-a2))*t + 0.5*(a1-a2)*t^2

-}


solveForCollisionTimes : Particle -> Particle -> (Triplet -> Int) -> List Float
solveForCollisionTimes p1 p2 component =
    let
        ( x1, v1, a1, x2, v2, a2 ) =
            ( toFloat <| component p1.p
            , toFloat <| component p1.v
            , toFloat <| component p1.a
            , toFloat <| component p2.p
            , toFloat <| component p2.v
            , toFloat <| component p2.a
            )

        a =
            0.5 * (a1 - a2)

        b =
            ((v1 - v2) + 0.5 * (a1 - a2))

        c =
            x1 - x2
    in
        quadratic a b c


quadratic : number -> number -> number -> List Float
quadratic a b c =
    if a == 0 then
        if b == 0 then
            -- constant
            []
        else
            -- linear
            [ -1 * c / b ]
    else
        -- quadratic
        let
            b2minus4ac =
                (b ^ 2) - (4 * a * c)
        in
            if b2minus4ac < 0 then
                -- Eliminate complex-valued solutions
                []
            else
                let
                    twoA =
                        2 * a

                    minusB =
                        -1 * b
                in
                    [ (minusB + sqrt b2minus4ac) / twoA
                    , (minusB - sqrt b2minus4ac) / twoA
                    ]


componentAtTime : Int -> Particle -> (Triplet -> Int) -> Int
componentAtTime tInt p component =
    let
        t =
            toFloat tInt

        ( x, v, a ) =
            ( toFloat <| component p.p
            , toFloat <| component p.v
            , toFloat <| component p.a
            )
    in
        round <|
            if t == 0 then
                x
            else
                x + (v * t) + (0.5 * t * (t + 1) * a)


positionAtTime : Int -> Particle -> Triplet
positionAtTime t p =
    { x = componentAtTime t p .x
    , y = componentAtTime t p .y
    , z = componentAtTime t p .z
    }


filterEvent : Particle -> Particle -> Int -> Maybe Event
filterEvent p1 p2 t =
    let
        triplet1 =
            positionAtTime t p1

        triplet2 =
            positionAtTime t p2
    in
        if triplet1 == triplet2 then
            Just ( t, triplet1.x, triplet1.y, triplet1.z )
        else
            Nothing


collisionEvents : Particle -> Particle -> List Event
collisionEvents p1 p2 =
    solveForCollisionTimes p1 p2 .x
        |> List.map round
        |> (\possibleTimes -> 0 :: possibleTimes)
        |> List.filterMap (filterEvent p1 p2)


insertOneCollision : Int -> Int -> Event -> Dict Event (Set Int) -> Dict Event (Set Int)
insertOneCollision id1 id2 event collidingIdsByEvent =
    Dict.update event
        (\maybeParticles ->
            maybeParticles
                |> Maybe.withDefault Set.empty
                |> Set.union (Set.fromList [ id1, id2 ])
                |> Just
        )
        collidingIdsByEvent


insertAllCollisionsForParticle : Dict Int Particle -> Int -> Particle -> Dict Event (Set Int) -> Dict Event (Set Int)
insertAllCollisionsForParticle particlesById id1 p1 collidingIdsByEvent =
    Dict.foldl
        (\id2 p2 accIdsByEvent ->
            if id1 == id2 then
                accIdsByEvent
            else
                let
                    events =
                        collisionEvents p1 p2
                in
                    List.foldl
                        (insertOneCollision id1 id2)
                        accIdsByEvent
                        events
        )
        collidingIdsByEvent
        particlesById


{-| Will find every collision twice, then eliminate using Set
Annoying. Could optimise but not bothering.
-}
findAllCollisions : Dict Int Particle -> Dict Event (Set Int)
findAllCollisions particlesById =
    Dict.foldl
        (insertAllCollisionsForParticle particlesById)
        Dict.empty
        particlesById


killParticles : Dict Int Particle -> Dict Event (Set Int) -> Dict Int Particle
killParticles particlesById collidingIdsByEvent =
    Dict.foldl
        (\collisionEvent collidingParticleIds accParticlesById ->
            let
                stillAliveParticleIds =
                    Set.filter
                        (\id -> Dict.member id accParticlesById)
                        collidingParticleIds
            in
                if Set.size stillAliveParticleIds < 2 then
                    accParticlesById
                else
                    Set.foldl
                        Dict.remove
                        accParticlesById
                        stillAliveParticleIds
        )
        particlesById
        collidingIdsByEvent


countParticlesAfterCollisions : List Particle -> Int
countParticlesAfterCollisions particleList =
    let
        particlesById =
            particleList
                |> List.indexedMap (,)
                |> Dict.fromList

        collidingIdsByEvent =
            findAllCollisions particlesById

        finalParticlesById =
            killParticles particlesById collidingIdsByEvent
    in
        Dict.size finalParticlesById


answer : String -> Result Parser.Error Int
answer input =
    Parser.run inputParser input
        |> Result.map countParticlesAfterCollisions
