module AnimalQuiz.State

open Elmish
open Types
open AnimalQuiz

let init () : Model * Cmd<Msg> =
  initState, []

let rec update msg model : Model * Cmd<Msg> =
  let newMessageHistory = model.MessageHistory @ [msg]
  match (msg,model.CurrentState) with
  | (NumOfReplay num,_ ) ->
    { model with NumberOfReplaySteps =  Some (num |> int); },[]
  | (InputStr str,_) ->
      { model with MessageFromPlayer=str;MessageHistory = newMessageHistory},[]
  | (Submit ,Welcome) ->
      { model with CurrentState = InviteToThinkAboutAnAnimal; MessageFromEngine = "Think about an animal"; MessageHistory = newMessageHistory},[]
  | (Submit ,InviteToThinkAboutAnAnimal) ->
      { model with CurrentState = GuessingFromCurrentNode; MessageFromEngine = Types.question model.CurrentNode; MessageHistory = newMessageHistory},[]

  | (No , GuessingFromCurrentNode) ->
      match model.CurrentNode with
      | AnimalName _ ->
        { model with CurrentState =  AskWhatAnimalWas; MessageFromEngine="What animal was?"; MessageHistory = newMessageHistory  },[]
      | SubTree X ->
        { model with 
            CurrentNode = X.NoBranch; 
            MessageFromEngine = Types.question X.NoBranch; YesNoList = model.YesNoList@["no"];
            MessageHistory = newMessageHistory },[]

  | (Yes , GuessingFromCurrentNode) ->
      match model.CurrentNode with
      | AnimalName _ ->
        { model with CurrentState =  Welcome; CurrentNode = model.RootTree; MessageFromEngine="yeah!";MessageHistory = newMessageHistory},[]
      | SubTree X -> 
        { model with CurrentNode = X.YesBranch; MessageFromEngine = Types.question X.YesBranch; YesNoList = model.YesNoList@["yes"]; MessageHistory = newMessageHistory },[]

  | (Submit , AskWhatAnimalWas) ->
      let currentAnimalName = match model.CurrentNode with 
      | SubTree X ->  failwith ("expecting a leaf with animal, got a tree with question: " + X.Question)
      | AnimalName X -> X 
      { model with 
         CurrentState = ExpectingDiscriminatingQuestion; 
         AnimalToBeLearned = Some model.MessageFromPlayer;
         MessageFromEngine="what question would you suggest to distinguish a "+model.MessageFromPlayer+ " from a " + currentAnimalName+ "?";
         MessageHistory= newMessageHistory},[]
  | (Submit , ExpectingDiscriminatingQuestion) ->
      let currentAnimal = match model.CurrentNode with | AnimalName X -> X | _ -> failwith "error inconsisetency"
      let theAnimalToBeLearned = match model.AnimalToBeLearned with | Some X -> X | _ -> failwith "error inconsistency"
      { model with 
            CurrentState = AnsweringDiscriminatingQuestion; 
            NewDiscriminatingQuestion = Some model.MessageFromPlayer;
            MessageFromEngine="what is the answer to the question \"" +
            model.MessageFromPlayer+"\""+"to distinguish a " +
            theAnimalToBeLearned+ " from a " + currentAnimal + " ?";
            MessageHistory = newMessageHistory },[]

  | (Yes, AnsweringDiscriminatingQuestion) ->
      let learnedTree = learn {model with MessageFromPlayer = "yes"}
      { model with CurrentState = Welcome; RootTree = learnedTree; CurrentNode = learnedTree;YesNoList = []; MessageHistory = newMessageHistory },[]
  | (No, AnsweringDiscriminatingQuestion) ->
      let learnedTree = learn {model with MessageFromPlayer = "no"}
      { model with CurrentState = Welcome; RootTree = learnedTree; CurrentNode = learnedTree;YesNoList = []; MessageHistory = newMessageHistory},[]

  | (Reset,_) -> initState,[]

  | (Replay,_) -> 
    {  model with CurrentState = SetReplayValue },[]
  | (DoReplay,_) ->
    match model.NumberOfReplaySteps with 
        | Some X -> 
            let msgHistoryTruncated = model.MessageHistory |> List.take X 
            msgHistoryTruncated |> List.fold (fun (acc,_) x -> update x  acc   )  (initState,[])
        | _ -> initState,[]

  | (Undo,_) -> 
     let msgHistoryTruncated = model.MessageHistory |> List.take (List.length model.MessageHistory - 1)
     msgHistoryTruncated |> List.fold (fun (acc,_) x -> update x  acc   )  (initState,[])
     
  
  
    





