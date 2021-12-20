module WorldTimeApi exposing (Data, abbreviation, get, name, zone)

import Http
import Json.Decode as D
import Time exposing (Zone, customZone)


type Data
    = Data
        { zone : Zone
        , name : String
        , abbreviation : String
        }


zone : Data -> Zone
zone (Data data) =
    data.zone


name : Data -> String
name (Data data) =
    data.name


abbreviation : Data -> String
abbreviation (Data data) =
    data.abbreviation


decoder : D.Decoder Data
decoder =
    D.map3
        (\offset name_ abbreviation_ ->
            Data
                { zone = customZone (offset // 60) []
                , name = name_
                , abbreviation = abbreviation_
                }
        )
        (D.field "raw_offset" D.int)
        (D.field "timezone" D.string)
        (D.field "abbreviation" D.string)


get : (Result Http.Error Data -> msg) -> String -> Cmd msg
get toMsg name_ =
    Http.get
        { url = "http://worldtimeapi.org/api/timezone/" ++ name_
        , expect =
            Http.expectJson
                toMsg
                decoder
        }
