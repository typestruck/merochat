module Test.Server.Wheel where
-- {-# Language OverloadedStrings #-}

-- import qualified Test.Hspec                    as TH
-- import qualified Lib                           as L
-- import           Lib                            ( Turn(..)
--                                                 , Stats(..)
--                                                 )
-- import qualified Test.QuickCheck               as TQ
-- import           Test.QuickCheck                ( Positive(..) )
-- import qualified Data.HashMap.Strict           as DHS
-- import           Data.Time.Clock                ( UTCTime(..) )
-- import qualified Data.Time.Clock               as DTC
-- import qualified Data.Time.Calendar            as DTCL

-- main :: IO ()
-- main = TH.hspec $ do
--     TH.describe "karmaFrom" $ do
--         let baseTurn = Turn
--                           { senderStats    = Stats { userID     = 1
--                                                    , characters = 1
--                                                    , interest   = 1
--                                                    }
--                           , recipientStats = Stats { userID     = 2
--                                                    , characters = 1
--                                                    , interest   = 1
--                                                    }
--                           , chatAge        = 1
--                           , replayDelay    = 1
--                           }

--         TH.it "sender gets more karma for equal inputs"
--             $ TQ.property
--             $ \(Positive x) ->
--                   let turn = Turn
--                           { senderStats    = Stats { userID     = 1
--                                                    , characters = x
--                                                    , interest   = x
--                                                    }
--                           , recipientStats = Stats { userID     = 2
--                                                    , characters = x
--                                                    , interest   = x
--                                                    }
--                           , chatAge        = x
--                           , replayDelay    = x
--                           }
--                   in fst (L.karmaFrom turn) > snd (L.karmaFrom turn)

--         TH.it "greater interest earns more karma"
--             $ TQ.property
--             $ \(Positive n) ->
--                   let turn = baseTurn { senderStats = (senderStats baseTurn) { interest = n}  }
--                       anotherTurn = baseTurn { senderStats = (senderStats baseTurn) { interest = n + n}  }
--                   in fst (L.karmaFrom turn) <= fst (L.karmaFrom anotherTurn)

--         TH.it "smaller reply delay earns more karma"
--             $ TQ.property
--             $ \(Positive n) ->
--                   let turn = baseTurn { replayDelay = n }
--                       anotherTurn = baseTurn { replayDelay = n + n }
--                   in fst (L.karmaFrom turn) >= fst (L.karmaFrom anotherTurn)

--         TH.it "smaller chat age earns more karma"
--             $ TQ.property
--             $ \(Positive n) ->
--                   let turn = baseTurn { chatAge = n }
--                       anotherTurn = baseTurn { chatAge = n + n }
--                   in fst (L.karmaFrom turn) >= fst (L.karmaFrom anotherTurn)

--     TH.describe "shouldSerialize" $ do
--         let
--             start = UTCTime { utctDay     = DTCL.fromGregorian 2000 1 1
--                             , utctDayTime = 0
--                             }

--         TH.it "hour and time match"
--             $ L.shouldSerialize 1 start (DTC.addUTCTime (60 * 60) start)
--             `TH.shouldBe` True
--         TH.it "hour greater than time"
--             $ L.shouldSerialize 2 start (DTC.addUTCTime (60 * 60 + 60) start)
--             `TH.shouldBe` False
--         TH.it "hour smaller than time"
--             $ L.shouldSerialize 3 start (DTC.addUTCTime (60 * 60 + 60) start)
--             `TH.shouldBe` False

--     TH.describe "parseTurn" $ do
--         TH.it "parses well formed input"
--             $             L.parseTurn "1,2,4;4,5,6;5,0"
--             `TH.shouldBe` Turn
--                               { senderStats    = Stats { userID     = 1
--                                                        , characters = 2.0
--                                                        , interest   = 4.0
--                                                        }
--                               , recipientStats = Stats { userID     = 4
--                                                        , characters = 5.0
--                                                        , interest   = 6.0
--                                                        }
--                               , chatAge        = 5
--                               , replayDelay    = 0
--                               }
--         TH.it "parses decimal input"
--             $             L.parseTurn "1,2.034,4.44444;4,0.0005,1.222226;5,0"
--             `TH.shouldBe` Turn
--                               { senderStats    = Stats { userID     = 1
--                                                        , characters = 2.034
--                                                        , interest   = 4.44444
--                                                        }
--                               , recipientStats = Stats { userID     = 4
--                                                        , characters = 0.0005
--                                                        , interest   = 1.222226
--                                                        }
--                               , chatAge        = 5
--                               , replayDelay    = 0
--                               }

--     TH.describe "runTurn" $ do
--         let turn = L.parseTurn "1,2,4;4,5,6;5,0"
--         TH.it "adds karma to state"
--             $             L.runTurn turn DHS.empty
--             `TH.shouldBe` DHS.fromList [(1, 5), (4, 4)]
--         TH.it "updates state karma"
--             $             L.runTurn turn (DHS.fromList [(1, 5), (4, 4)])
--             `TH.shouldBe` DHS.fromList [(1, 10), (4, 8)]

--     TH.describe "makeInsert" $ do
--         TH.it "generates sql insert"
--             $ L.makeInsert (DHS.fromList [(1, 5), (4, 4)])
--             `TH.shouldBe` "insert into karma_histories (target, amount) values(1,5),(4,4)"

