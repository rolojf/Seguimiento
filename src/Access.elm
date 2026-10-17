module Access exposing (user, verify)

import Crypto.Hash
import Domain exposing (User(..))
import Env
import Types exposing (Session)


verify : String -> String -> Maybe Session
verify username key =
    let
        account =
            case String.toLower (String.trim username) of
                "usuario1" ->
                    Just UserOne

                "usuario2" ->
                    Just UserTwo

                _ ->
                    Nothing
    in
    -- Keys contain 32 random bytes encoded as base64url, not chosen passwords.
    if String.length key == 43 && String.all keyChar key then
        account
            |> Maybe.andThen
                (\candidate ->
                    let
                        expected =
                            keyHash candidate
                    in
                    if String.length expected == 64 && Crypto.Hash.sha256 key == expected then
                        Just { user = candidate, keyHash = expected }

                    else
                        Nothing
                )

    else
        Nothing


user : Session -> Maybe User
user session =
    -- Replacing a key also revokes sessions created with the previous key.
    if String.length session.keyHash == 64 && session.keyHash == keyHash session.user then
        Just session.user

    else
        Nothing


keyHash : User -> String
keyHash account =
    case account of
        UserOne ->
            Env.userOneKeyHash

        UserTwo ->
            Env.userTwoKeyHash


keyChar : Char -> Bool
keyChar char =
    Char.isAlphaNum char || char == '-' || char == '_'
