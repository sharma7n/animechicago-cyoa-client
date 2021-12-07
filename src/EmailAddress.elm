module EmailAddress exposing
    ( Entry(..)
    , toString
    , validate
    )

import Validate

type Entry
    = Blank
    | Invalid String
    | Valid String

toString : Entry -> String
toString e =
    case e of
        Blank ->
            ""
        
        Invalid s ->
            s
        
        Valid s ->
            s

validate : String -> Entry
validate s =
    if Validate.isValidEmail s then
        Valid s
    else if s == "" then
        Blank
    else
        Invalid s