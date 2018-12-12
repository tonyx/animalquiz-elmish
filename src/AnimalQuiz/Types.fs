module AnimalQuiz.Types

type State = | Welcome | InviteToThinkAboutAnAnimal |  GuessingFromCurrentNode |AskWhatAnimalWas | ExpectingDiscriminatingQuestion | AnsweringDiscriminatingQuestion | SetReplayValue

type KnowledgeTree = AnimalName of string | SubTree of Tree
and Tree = {Question: string; YesBranch: KnowledgeTree; NoBranch: KnowledgeTree}


let question tree  =
    match tree with
        | AnimalName name -> "is it a "+name+"?"
        | SubTree {Question=question} -> question 

type Msg =
  | InputStr of string
  | Submit
  | Yes
  | No
  | Reset
  | Replay
  | NumOfReplay of string
  | DoReplay 
  | Undo 


type Model = { MessageFromPlayer: string; 
               NumberOfReplaySteps: int option;
               CurrentState:State;
               RootTree: KnowledgeTree;
               CurrentNode: KnowledgeTree;
               MessageFromEngine: string;
               AnimalToBeLearned: string Option;
               YesNoList: string list;
               NewDiscriminatingQuestion: string option;
               MessageHistory: Msg list;
        }

let initState =   {
      MessageFromPlayer="";
      NumberOfReplaySteps = None;
      CurrentState=Welcome;
      RootTree = AnimalName "elephant";
      CurrentNode = AnimalName "elephant";
      MessageFromEngine = "Welcome";
      NewDiscriminatingQuestion = None;
      AnimalToBeLearned= None;
      YesNoList = [];
      MessageHistory = [];
  }


// let mutable commandHistory: Msg list = []

let rec learn model = 

    match model.YesNoList with 
    | [] -> 
            let theDiscriminationgQuestion =  match model.NewDiscriminatingQuestion with
                | Some X -> X
                | _ -> failwith "error, discriminationg question cannot be None"
            let theAnimalToBeLearned = match model.AnimalToBeLearned with
                | Some X -> X
                | _ -> failwith "error, the animal cannot be None"
            let currentAnimal = match model.CurrentNode with
                |  AnimalName X -> X 
                | _ -> failwith "error. The tree should be leaf here"

            match model.MessageFromPlayer with
            |  "yes" -> 
                SubTree {
                    Question = theDiscriminationgQuestion;
                    YesBranch = AnimalName theAnimalToBeLearned;
                    NoBranch = AnimalName currentAnimal;
                }
            |  "no" -> SubTree {
                    Question = theDiscriminationgQuestion;
                    YesBranch = AnimalName currentAnimal;
                    NoBranch = AnimalName theAnimalToBeLearned;
                }
            | _ -> failwith "expcting a no and yes in the yesnolist"
                
    | "yes"::T ->
        let nodeTree = match model.RootTree with
           | SubTree X -> X
           | _ -> failwith "subtree must be not leaf at this point"

        let nodeQuestion = nodeTree.Question

        let yesBranch = 
            nodeTree.YesBranch

        let noBranch = 
            nodeTree.NoBranch

        SubTree {
                    Question = nodeQuestion;
                    YesBranch = learn {model with RootTree = yesBranch; YesNoList=T};
                    NoBranch = noBranch
                }
    | "no"::T -> 
        let nodeTree = match model.RootTree with
           | SubTree X -> X
           | _ -> failwith "subtree must be not leaf at this point"

        let nodeQuestion = nodeTree.Question

        let yesBranch = 
            nodeTree.YesBranch

        let noBranch = 
            nodeTree.NoBranch

        SubTree {
                    Question = nodeQuestion;
                    YesBranch = yesBranch;
                    NoBranch = learn {model with RootTree = noBranch;YesNoList=T}
                }






