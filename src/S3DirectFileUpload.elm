module S3DirectFileUpload exposing
  ( FileUpload
  , Metadata
  , upload
  , fileUploadDecoder
  , metadataDecoder
  )

import Dict exposing (Dict)
import File exposing (File)
import Http exposing (header, filePart, stringPart)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Task exposing (Task)
import Url.Builder as Builder

type alias FileUpload =
  { id : String
  , storage : String
  , metadata : Metadata
  }

type alias Metadata =
  { size : Int
  , filename : String
  , mime_type : String
  }

type alias UploadSettings =
  { method : String
  , url : String
  , fields : SigningData
  , headers : Dict String String
  }

type alias SigningData =
  { key : String
  , policy : String
  , credential : String
  , algorithm : String
  , date : String
  , signature : String
  , disposition : String
  , contentType : String
  }

type DataError = NoSettings | NoFile
type Error = DataError DataError

fileUploadDecoder : Decoder FileUpload
fileUploadDecoder =
  map3 FileUpload
    (field "id" string)
    (field "storage" string)
    (field "metadata" metadataDecoder)

metadataDecoder : Decoder Metadata
metadataDecoder =
  map3 Metadata
    (field "size" int)
    (field "filename" string)
    (field "mime_type" string)

uploadSettingsDecoder : Decoder UploadSettings
uploadSettingsDecoder =
  map4 UploadSettings
    (field "method" string)
    (field "url" string)
    (field "fields" signingDecoder)
    (field "headers" (dict string))

signingDecoder : Decoder SigningData
signingDecoder =
  map8 SigningData
    (field "key" string)
    (field "policy" string)
    (field "x-amz-credential" string)
    (field "x-amz-algorithm" string)
    (field "x-amz-date" string)
    (field "x-amz-signature" string)
    (field "Content-Disposition" string)
    (field "Content-Type" string)

handleJsonResponse : Decoder response -> Http.Response String -> Result Http.Error response
handleJsonResponse decoder response =
  case response of
    Http.BadUrl_ url ->
      Err (Http.BadUrl url)

    Http.Timeout_ ->
      Err Http.Timeout

    Http.BadStatus_ { statusCode } _ ->
      Err (Http.BadStatus statusCode)

    Http.NetworkError_ ->
      Err Http.NetworkError

    Http.GoodStatus_ _ body ->
      case Decode.decodeString decoder body of
        Err _ ->
          Err (Http.BadBody body)

        Ok result ->
          Ok result

handleStringResponse : Http.Response String -> Result Http.Error String
handleStringResponse response =
  case response of
    Http.BadUrl_ url ->
      Err (Http.BadUrl url)

    Http.Timeout_ ->
      Err Http.Timeout

    Http.BadStatus_ { statusCode } _ ->
      Err (Http.BadStatus statusCode)

    Http.NetworkError_ ->
      Err Http.NetworkError

    Http.GoodStatus_ _ body ->
      Ok body

getSettings : File -> String -> Task Http.Error UploadSettings
getSettings file signingEndpoint =
  let
    filename =
      File.name file

    filetype =
      File.mime file

    settingsUrl =
      Builder.absolute [ signingEndpoint ]
        [ Builder.string "filename" filename
        , Builder.string "type" filetype
        ]
  in
    Http.task
      { method = "GET"
      , headers = []
      , url = settingsUrl
      , body = Http.emptyBody
      , resolver = Http.stringResolver <| handleJsonResponse <| uploadSettingsDecoder
      , timeout = Nothing
      }

upload : File -> String -> Task Http.Error FileUpload
upload file signingEndpoint =
  getSettings file signingEndpoint
    |> Task.andThen
         (\settings ->
           uploadFile settings file
           |> Task.andThen
                (\_ ->
                  Task.succeed (buildFileUpload settings file)
                )
         )

-- For field references: https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-HTTPPOSTForms.html

uploadFile : UploadSettings -> File -> Task Http.Error String
uploadFile uploadSettings file =
  let
    fields = uploadSettings.fields
    headers = uploadSettings.headers
    body =
      Http.multipartBody
        [ stringPart "key" fields.key
        , stringPart "policy" fields.policy
        , stringPart "content-type" fields.contentType
        , stringPart "content-disposition" fields.disposition
        , stringPart "x-amz-credential" fields.credential
        , stringPart "x-amz-algorithm" fields.algorithm
        , stringPart "x-amz-date" fields.date
        , stringPart "x-amz-signature" fields.signature
        , filePart "file" file
        ]
  in
    Http.task
      { method = uploadSettings.method
      , headers = []
      , url = uploadSettings.url
      , body = body
      , resolver = Http.stringResolver <| handleStringResponse
      , timeout = Nothing
      }

buildFileUpload : UploadSettings -> File -> FileUpload
buildFileUpload settings file =
  let
    keyParts = String.split "/" settings.fields.key
    id =
      Maybe.withDefault settings.fields.key (List.head <| Maybe.withDefault [] (List.tail keyParts))
    storage =
      Maybe.withDefault "" (List.head keyParts)
  in
    { id = id
    , storage = storage
    , metadata =
      { size = File.size file
      , filename = File.name file
      , mime_type = File.mime file
      }
    }

