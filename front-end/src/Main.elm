import Effects exposing (Never)
import App exposing (init, update, view)
import StartApp
import Task
import Interop exposing (interopMailbox)


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

port interop : Signal String
port interop = interopMailbox.signal