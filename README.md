# S3 Direct File Uploads in Elm

This is a simple library that allows you to upload files directly to Amazon S3 (or compatible services, except Digital Ocean Spaces which will rate limit you immediately).

## Usage

I've basically ripped this out of the [Senryu.pub](https://senryu.pub) editor for the moment, so documentation is
a bit thin on the ground, however the following code should be most of what is needed to get it working.

```elm
  -- Import this library
  import S3DirectFileUpload exposing (FileUpload)

  -- Other dependencies for the example code below
  import File exposing (File)
  import File.Select as Select
  import Html.Events exposing (onClick)
  import Task

  -- Example upload button in the view:
  uploadButton : Model -> Html Message
  uploadButton model =
    a [ class "btn", onClick Pick ]
      [ text "Upload images" ]


  update : Message -> Model -> (Model, Cmd Message)
  update message model =
    Pick ->
      ( model
      , Select.files ["image/*"] GotFiles
      )

    GotFiles file files ->
      let
        allFiles = (file :: files)

        commands =
          List.map
            (\file_ ->
              -- "media/sign" here is the endpoint on the application which generates some needed
              -- params for this to work. Shrine defaults to "/s3/params" when mounted, but you
              -- need it without the preceding / here, e.g. "s3/params"
              Task.attempt GotFileUpload (FileUpload.upload file_ "media/sign")
            )
            allFiles

      in
        ( model
        , Cmd.batch commands
        )

    GotFileUpload fileUpload ->
      case fileUpload of
        Err err ->
          -- Most likely a HTTP error
          -- Do something with the error, e.g.
          ( { model | state = Broken }, Cmd.none )
        Ok data ->
          -- Do something with the returned FileUpload type, e.g.
          saveToServer data
```
