module Shell where
  
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, error, makeAff, throwError)
import Node.ChildProcess
    (ExecResult
    , defaultExecOptions
    , execFile
    , kill, stdin)
import Prelude
import Node.Stream (Readable, pipe)

execAsync ::
    Maybe String
    -> String
    -> Maybe (Readable ())
    -> Aff ExecResult
execAsync cwd cmd maybeInput =
  case
    cmd 
      # String.split (Pattern " ")
      # Array.filter ((/=) "")
      # Array.uncons
  of
    Just { head : name, tail : args } ->
      makeAff (\callback -> do
        let option = defaultExecOptions
                      { cwd = cwd
                      , timeout = Just 2000.0
                      --, windowsHide = true
                      }
            onAfterExecute :: ExecResult -> Effect Unit
            onAfterExecute result = 
                callback $ Right result

        child <- execFile name args option $ onAfterExecute
        case maybeInput of
             Just input ->
                 void $ pipe input (stdin child)
             _ ->
                 pure unit
        pure $ effectCanceler $ kill SIGINT child
      )

    _ ->
      throwError $ error $ "invalid command: " <> cmd
