module AnimalQuiz.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types


let root (model:Model) dispatch =

   let numSteps = if (model.NumberOfReplaySteps.IsSome) then model.NumberOfReplaySteps.Value else 0

   div

    [ ]

    [ p
        [ ClassName "control" ]
        [ 
          span
           [ ]
           [ 
            str (sprintf "%s  " model.MessageFromEngine )
           ]

          (if ((model.CurrentState<> Welcome && model.CurrentState <> InviteToThinkAboutAnAnimal
             && model.CurrentState <> GuessingFromCurrentNode && model.CurrentState <> AnsweringDiscriminatingQuestion ) ) then
          input
            [ ClassName "input"
              Type "text"
              Id "hi"
              Placeholder "Write your message here"
              DefaultValue model.MessageFromPlayer
              AutoFocus true
              OnChange (fun ev -> !!ev.target?value |> InputStr |> dispatch ) 
            ] 
            else 
          div [] [])
          br []

          (if (model.CurrentState = GuessingFromCurrentNode || 
            model.CurrentState = AnsweringDiscriminatingQuestion) then
          div [ ClassName "column is-narrow" ]
            [ 
              a
                [ ClassName "button"
                  OnClick (fun _ -> Yes |> dispatch) 
                ]
               [ str "Yes" ] 
              a [ ClassName "button"
                  OnClick (fun _ -> No |> dispatch) 
                ]
               [ str "No" ] 
              // a [ ClassName "button"
              //     OnClick (fun _ -> No |> dispatch) 
              //   ]
              //  [ str "Replay" ] 
            ]
          else div [] []
          )


          (if (model.CurrentState <> GuessingFromCurrentNode && model.CurrentState <> 
            AnsweringDiscriminatingQuestion)  
            
            then
              div
                [ ClassName "column is-narrow" ]
                [ a
                    [ ClassName "button"
                      OnClick (fun _ -> Submit |> dispatch) 
                    ]
                [ str "submit" ] ]
                else div [] []
          )

          div
            [ ClassName "column is-narrow" ]
            [ a
                [ ClassName "button"
                  OnClick (fun _ -> Undo |> dispatch) 
                ]
            [ str "Undo" ] ]

          div
            [ ClassName "column is-narrow" ]
            [ a
                [ ClassName "button"
                  OnClick (fun _ -> Reset |> dispatch) 
                ]
            [ str "Reset" ] ]

          div
            [ ClassName "column is-narrow" ]
            [ a
                [ ClassName "button"
                  OnClick (fun _ -> Replay |> dispatch) 
                ]
            [ str "Replay" ] ]
           
          br []

          (if (model.CurrentState = SetReplayValue)
             then
            div [ClassName "column is-narrow"]
              [
                input
                  [ ClassName "input"
                    Type "number"
                    Id "replayValue"
                    Placeholder  (sprintf "%d\n" model.MessageHistory.Length)
                    DefaultValue (sprintf "%d\n" model.MessageHistory.Length)
                    AutoFocus true
                    OnChange (fun ev -> !!ev.target?value |> NumOfReplay |> dispatch ) 
                  ] 

                a 
                  [ ClassName "button"
                    OnClick (fun _  -> DoReplay  |> dispatch) 
                  ] 
                  [str "Do replay"]

              ]


              else div [] []

          )

        ]
      br []

      span
        [ ]
        [ 
          str (sprintf "%s   " model.MessageFromPlayer ) 
        ] 

      br []

      span
        [ ]
        [ 
          str (sprintf "%s " (model.CurrentState.ToString()))  
        ] 
      span
        [ ]
        [ 
            str (sprintf " number of steps %d " numSteps)
        ] 
      ]


      

