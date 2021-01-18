module Uml

import Data.Maybe
import Data.List
import Data.List1
import Data.Strings
import System
import System.File

import Pfsm
import Pfsm.Analyser
import Pfsm.Checker
import Pfsm.Data
import Pfsm.Parser

umlKeywords : List String
umlKeywords = [ "as", "enduml", "startuml", "state" ]

toUmlName : Name -> String
toUmlName n
  = let n' = normalize n in
    if elemBy (==) n' umlKeywords
       then "my_" ++ n'
       else n'
  where
    mappings : List (String, String)
    mappings =  [ (" ", "_")
                , ("-", "_")
                , ("+", "plus")
                ]
    normalize : Name -> String
    normalize n = foldl (\acc, x => replaceAll (fst x) (snd x) acc) n mappings

toUmlModelAttribute : String -> String
toUmlModelAttribute "@" = "model"
toUmlModelAttribute a
  = if isPrefixOf "@" a
       then "model." ++ toUmlName (substr 1 (minus (length a) 1) a)
       else toUmlName a

toUmlExpression : String -> Expression -> String
toUmlExpression _ (ApplicationExpression n es) = (toUmlName n) ++ "(" ++ (join ", " (map (toUmlExpression "") es)) ++ ")"
toUmlExpression _ (BooleanExpression True)     = "true"
toUmlExpression _ (BooleanExpression False)    = "false"
toUmlExpression _ (IdentifyExpression i)       = toUmlModelAttribute i
toUmlExpression _ (IntegerLiteralExpression i) = show i
toUmlExpression _ (RealLiteralExpression r)    = show r
toUmlExpression _ (CharLiteralExpression c)    = show c
toUmlExpression _ (StringLiteralExpression s)  = "\"" ++ s ++ "\""

toUmlCompareOperation : CompareOperation -> String
toUmlCompareOperation NotEqualsToOperation         = "!="
toUmlCompareOperation EqualsToOperation            = "=="
toUmlCompareOperation LessThanOperation            = "<"
toUmlCompareOperation LessThanOrEqualsToOperation  = "<="
toUmlCompareOperation GreatThanOperation           = ">"
toUmlCompareOperation GreatThanOrEqualsToOperation = ">="

toUmlTestExpression : TestExpression -> String
toUmlTestExpression (PrimitiveTestExpression e)     = toUmlExpression "" e
toUmlTestExpression (BinaryTestExpression op e1 e2) = (toUmlTestExpression e1) ++ " " ++ (show op) ++ " " ++ (toUmlTestExpression e2)
toUmlTestExpression (UnaryTestExpression op e)      = (show op) ++ " " ++ (toUmlTestExpression e)
toUmlTestExpression (CompareExpression op e1 e2)    = (toUmlExpression "" e1) ++ " " ++ (toUmlCompareOperation op) ++ " " ++ (toUmlExpression "" e2)

toUml : Fsm -> String
toUml fsm
  = List.join "\n\n" [ "@startuml"
                     , generateStates fsm
                     , "[*] --> state" ++ (show (fromMaybe 0 (fromMaybe (Just Z) (map (\x => Data.List1.index (the State x) fsm.states) (startState fsm)))))
                     , generateTransitions fsm
                     , "@enduml"
                     ]
  where
    generateState : State -> Nat -> String
    generateState (MkState name _ _ metas) i
      = "state \"" ++ (displayName name metas) ++ "\" as state" ++ (show i)

    generateStates : Fsm -> String
    generateStates fsm = join "\n" $ map (\(i, x) => generateState x i) $ enumerate fsm.states

    generateTrigger : Trigger -> String
    generateTrigger (MkTrigger ps e (Just g) _) = (join "/" (map (.name) ps)) ++ " ☛ " ++ (displayName e.name e.metas) ++ " (" ++ toUmlTestExpression g ++ ")"
    generateTrigger (MkTrigger ps e Nothing  _) = (join "/" (map (.name) ps)) ++ " ☛ " ++ (displayName e.name e.metas)

    generateTransition : List1 State -> Transition -> String
    generateTransition ss (MkTransition s d ts)
      = let si = fromMaybe 0 $ Data.List1.index s ss
            di = fromMaybe 0 $ Data.List1.index d ss
            triggers = List1.join "\\n----\\n" $ map generateTrigger ts in
            "state" ++ (show si) ++ " --> state" ++ (show di) ++ " : " ++ triggers

    generateTransitions : Fsm -> String
    generateTransitions fsm = List1.join "\n" $ map (\x => generateTransition fsm.states x) fsm.transitions

doWork : String -> IO ()
doWork src
  = do Right fsm <- loadFsmFromFile src
       | Left err => putStrLn $ show err
       putStrLn $ toUml fsm

usage : IO ()
usage
  = putStrLn "Usage: pfsm-to-uml <src>"

main : IO ()
main
  = do
    args <- getArgs
    case args of
         x0 :: x1 :: [] => doWork x1
         _ => usage
