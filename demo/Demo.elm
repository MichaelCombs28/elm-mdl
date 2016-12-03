import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Html.Lazy
import Platform.Cmd exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import String
import Debug
import Task

import Navigation
import Navigation.Router as Router exposing (UrlChange, HistoryEntry(..))

import UrlParser exposing ((</>))

import Material
import Material.Color as Color
import Material.Layout as Layout
import Material.Helpers exposing (pure, lift, map1st, map2nd)
import Material.Options as Options exposing (css, when)
import Material.Scheme as Scheme
import Material.Icon as Icon
import Material.Typography as Typography
import Material.Menu as Menu

import Demo.Buttons
import Demo.Menus
import Demo.Tables
import Demo.Grid
import Demo.Textfields
import Demo.Snackbar
import Demo.Badges
import Demo.Elevation
import Demo.Toggles
import Demo.Loading
import Demo.Layout
import Demo.Footer
import Demo.Tooltip
import Demo.Tabs
import Demo.Slider
import Demo.Typography
import Demo.Cards
import Demo.Lists
import Demo.Dialog
import Demo.Chips
--import Demo.Template


-- MODEL



type alias Model =
  { router : Router.Model
  , history : List Navigation.Location
  , mdl : Material.Model
  , buttons : Demo.Buttons.Model
  , badges : Demo.Badges.Model
  , layout : Demo.Layout.Model
  , menus : Demo.Menus.Model
  , textfields : Demo.Textfields.Model
  , toggles : Demo.Toggles.Model
  , snackbar : Demo.Snackbar.Model
  , tables : Demo.Tables.Model
  , loading : Demo.Loading.Model
  , footers : Demo.Footer.Model
  , tooltip : Demo.Tooltip.Model
  , tabs : Demo.Tabs.Model
  , slider : Demo.Slider.Model
  , typography : Demo.Typography.Model
  , cards : Demo.Cards.Model
  , lists : Demo.Lists.Model
  , dialog : Demo.Dialog.Model
  , elevation : Demo.Elevation.Model
  , chips : Demo.Chips.Model
  --, template : Demo.Template.Model
  , selectedTab : Int
  , transparentHeader : Bool
  }


initModel : Navigation.Location -> Model
initModel location =
  { router = Router.init location
  , history = [] -- [ location ]
  , mdl = Layout.setTabsWidth 2124 Material.model
  , buttons = Demo.Buttons.model
  , badges = Demo.Badges.model
  , layout = Demo.Layout.model
  , menus = Demo.Menus.model
  , textfields = Demo.Textfields.model
  , toggles = Demo.Toggles.model
  , snackbar = Demo.Snackbar.model
  , tables = Demo.Tables.model
  , loading = Demo.Loading.model
  , footers = Demo.Footer.model
  , tooltip = Demo.Tooltip.model
  , tabs = Demo.Tabs.model
  , slider = Demo.Slider.model
  , typography = Demo.Typography.model
  , cards = Demo.Cards.model
  , lists = Demo.Lists.model
  , dialog = Demo.Dialog.model
  , elevation = Demo.Elevation.model
  , chips = Demo.Chips.model
  --, template = Demo.Template.model
  , selectedTab = 0
  , transparentHeader = False
  }



-- ACTION, UPDATE


type Msg
  = LocationChanged Navigation.Location
  | SelectTab Int
  | Mdl (Material.Msg Msg)
  | BadgesMsg Demo.Badges.Msg
  | ButtonsMsg Demo.Buttons.Msg
  | LayoutMsg Demo.Layout.Msg
  | MenusMsg Demo.Menus.Msg
  | TextfieldMsg Demo.Textfields.Msg
  | SnackbarMsg Demo.Snackbar.Msg
  | TogglesMsg Demo.Toggles.Msg
  | TablesMsg Demo.Tables.Msg
  | LoadingMsg Demo.Loading.Msg
  | FooterMsg Demo.Footer.Msg
  | TooltipMsg Demo.Tooltip.Msg
  | TabMsg Demo.Tabs.Msg
  | SliderMsg Demo.Slider.Msg
  | TypographyMsg Demo.Typography.Msg
  | CardsMsg Demo.Cards.Msg
  | ListsMsg Demo.Lists.Msg
  | ToggleHeader
  | DialogMsg Demo.Dialog.Msg
  | ElevationMsg Demo.Elevation.Msg
  | ChipMsg Demo.Chips.Msg
  --| TemplateMsg Demo.Template.Msg


nth : Int -> List a -> Maybe a
nth k xs =
  List.drop k xs |> List.head


{-| Our update function separates the processing of LocationChanged
messages from the processing of other, "internal" messages.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      LocationChanged location ->
        let
          ( newRouter, external ) =
            Router.locationChanged model.router location

              -- We keep the location history as an example, but
              -- don't change our URL based on the history.
          newModel =
            { model
              | history = location :: model.history
              , router = newRouter
            }
        in
            Router.processLocation external
              update location2messages location newModel []

          -- Non-location messages that may change model state
          -- call a sub-function that returns a new model with
          -- a router updated from the `Router.urlChanged` function.
      _ ->
        let
          ( newModel, cmd, mightChangeUrl ) = updateModelState msg model
        in
            if mightChangeUrl then
              let
                ( newRouter, routerCmd ) =
                  Router.urlChanged model.router (delta2url model newModel)
              in
                  ( { newModel | router = newRouter }
                  , Cmd.batch [ cmd, routerCmd ]
                  )
            else
              ( newModel, cmd )



updateModelState : Msg -> Model -> ( Model, Cmd Msg, Bool )
updateModelState action model =
  case action of
    SelectTab k ->
      ( { model | selectedTab = k } , Cmd.none, True )

    ToggleHeader ->
      ( { model | transparentHeader = not model.transparentHeader }, Cmd.none, False)

    Mdl msg ->
      noUrlChange (Material.update msg model)

    ButtonsMsg     a -> lift  .buttons    (\m x->{m|buttons       =x}) ButtonsMsg  Demo.Buttons.update       a model |> noUrlChange
    BadgesMsg      a -> lift  .badges     (\m x->{m|badges        =x}) BadgesMsg   Demo.Badges.update        a model |> noUrlChange
    LayoutMsg      a -> lift  .layout     (\m x->{m|layout        =x}) LayoutMsg  Demo.Layout.update         a model |> noUrlChange
    MenusMsg       a -> lift  .menus      (\m x->{m|menus         =x}) MenusMsg  Demo.Menus.update           a model |> noUrlChange
    TextfieldMsg m ->
      Demo.Textfields.update m model.textfields
        |> Maybe.map (map1st (\x -> { model | textfields = x }))
        |> Maybe.withDefault (model, Cmd.none)
        |> map2nd (Cmd.map TextfieldMsg)
        |> noUrlChange
    SnackbarMsg    a -> lift  .snackbar     (\m x->{m|snackbar    =x}) SnackbarMsg Demo.Snackbar.update      a model |> noUrlChange
    TogglesMsg     a -> lift .toggles       (\m x->{m|toggles     =x}) TogglesMsg Demo.Toggles.update        a model |> noUrlChange
    TablesMsg      a -> lift  .tables       (\m x->{m|tables      =x}) TablesMsg  Demo.Tables.update         a model |> noUrlChange
    LoadingMsg     a -> lift  .loading      (\m x->{m|loading     =x}) LoadingMsg  Demo.Loading.update       a model |> noUrlChange
    FooterMsg      a -> lift  .footers      (\m x->{m|footers     =x}) FooterMsg  Demo.Footer.update         a model |> noUrlChange
    SliderMsg      a -> lift  .slider       (\m x->{m|slider      =x}) SliderMsg  Demo.Slider.update         a model |> noUrlChange
    TooltipMsg     a -> lift  .tooltip      (\m x->{m|tooltip     =x}) TooltipMsg  Demo.Tooltip.update       a model |> noUrlChange
    TabMsg         a -> lift  .tabs         (\m x->{m|tabs        =x}) TabMsg  Demo.Tabs.update              a model |> noUrlChange
    TypographyMsg  a -> lift  .typography   (\m x->{m|typography  =x}) TypographyMsg Demo.Typography.update  a model |> noUrlChange
    CardsMsg       a -> lift  .cards        (\m x->{m|cards       =x}) CardsMsg  Demo.Cards.update           a model |> noUrlChange
    ListsMsg       a -> lift  .lists        (\m x->{m|lists       =x}) ListsMsg  Demo.Lists.update           a model |> noUrlChange
    DialogMsg      a -> lift .dialog        (\m x->{m|dialog      =x}) DialogMsg Demo.Dialog.update          a model |> noUrlChange
    ElevationMsg   a -> lift .elevation     (\m x->{m|elevation   =x}) ElevationMsg Demo.Elevation.update    a model |> noUrlChange
    ChipMsg        a -> lift .chips         (\m x->{m|chips       =x}) ChipMsg Demo.Chips.update             a model |> noUrlChange
    --TemplateMsg  a -> lift  .template   (\m x->{m|template  =x}) TemplateMsg Demo.Template.update   a model
    LocationChanged a -> (model, Cmd.none, False)


noUrlChange : ( Model, Cmd Msg ) -> ( Model, Cmd Msg, Bool )
noUrlChange ( model, cmd ) =
  ( model , cmd, False )

-- VIEW


tabs : List (String, String, Model -> Html Msg)
tabs =
  [ ("Buttons", "buttons", .buttons >> Demo.Buttons.view >> Html.map ButtonsMsg)
  , ("Badges", "badges", .badges >> Demo.Badges.view >> Html.map BadgesMsg)
  , ("Cards", "cards", .cards >> Demo.Cards.view >> Html.map CardsMsg)
  , ("Chips", "chips", .chips >> Demo.Chips.view >> Html.map ChipMsg)
  , ("Dialog", "dialog", .dialog >> Demo.Dialog.view >> Html.map DialogMsg)
  , ("Elevation", "elevation", .elevation >> Demo.Elevation.view >> Html.map ElevationMsg)
  , ("Footers", "footers", .footers >> Demo.Footer.view >> Html.map FooterMsg)
  , ("Grid", "grid", \_ -> Demo.Grid.view)
  , ("Layout", "layout", .layout >> Demo.Layout.view >> Html.map LayoutMsg)
  , ("Lists", "lists", .lists >> Demo.Lists.view >> Html.map ListsMsg)
  , ("Loading", "loading", .loading >> Demo.Loading.view >> Html.map LoadingMsg)
  , ("Menus", "menus", .menus >> Demo.Menus.view >> Html.map MenusMsg)
  , ("Sliders", "sliders", .slider >> Demo.Slider.view >> Html.map SliderMsg)
  , ("Snackbar", "snackbar", .snackbar >> Demo.Snackbar.view >> Html.map SnackbarMsg)
  , ("Tables", "tables", .tables >> Demo.Tables.view >> Html.map TablesMsg)
  , ("Tabs", "tabs", .tabs >> Demo.Tabs.view >> Html.map TabMsg)
  , ("Textfields", "textfields", .textfields >> Demo.Textfields.view >> Html.map TextfieldMsg)
  , ("Toggles", "toggles", .toggles >> Demo.Toggles.view >> Html.map TogglesMsg)
  , ("Tooltips", "tooltips", .tooltip >> Demo.Tooltip.view >> Html.map TooltipMsg)
  , ("Typography", "typography", .typography >> Demo.Typography.view >> Html.map TypographyMsg)
  --, ("Template", "template", .template >> Demo.Template.view >> Html.map TemplateMsg)
  ]


tabTitles : List (Html a)
tabTitles =
  List.map (\(x,_,_) -> text x) tabs


tabViews : Array (Model -> Html Msg)
tabViews = List.map (\(_,_,v) -> v) tabs |> Array.fromList


tabUrls : Array String
tabUrls =
  List.map (\(_,x,_) -> x) tabs |> Array.fromList


urlTabs : Dict String Int
urlTabs =
  List.indexedMap (\idx (_,x,_) -> (x, idx)) tabs |> Dict.fromList


e404 : Model -> Html Msg
e404 _ =
  div
    [
    ]
    [ Options.styled Html.h1
        [ Options.cs "mdl-typography--display-4"
        , Typography.center
        ]
        [ text "404" ]
    ]



drawer : List (Html Msg)
drawer =
  [ Layout.title [] [ text "Example drawer" ]
  , Layout.navigation
    []
    [ Layout.link
        [ Layout.href "https://github.com/debois/elm-mdl" ]
        [ text "github" ]
    , Layout.link
        [ Layout.href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
        [ text "elm-package" ]
    , Layout.link
        [ Layout.href "#cards"
        , Layout.onClick (Layout.toggleDrawer Mdl)
        ]
        [ text "Card component" ]
    ]
  ]


header : Model -> List (Html Msg)
header model =
  if model.layout.withHeader then
    [ Layout.row
        [ if model.transparentHeader then css "height" "192px" else Options.nop
        , css "transition" "height 333ms ease-in-out 0s"
        ]
        [ Layout.title [] [ text "elm-mdl" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [ Layout.onClick ToggleHeader]
                [ Icon.i "photo" ]
            , Layout.link
                [ Layout.href "https://github.com/debois/elm-mdl"]
                [ span [] [text "github"] ]
            , Layout.link
                [ Layout.href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
                [ text "elm-package" ]
            ]
        ]
    ]
  else
    []


view : Model -> Html Msg
view = Html.Lazy.lazy view_


view_ : Model -> Html Msg
view_ model =
  let
    top =
      (Array.get model.selectedTab tabViews |> Maybe.withDefault e404) model
  in
    Layout.render Mdl model.mdl
      [ Layout.selectedTab model.selectedTab
      , Layout.onSelectTab SelectTab
      , model.layout.fixedHeader |> when Layout.fixedHeader
      , model.layout.fixedDrawer |> when Layout.fixedDrawer
      , model.layout.fixedTabs |> when Layout.fixedTabs
      , model.layout.withHeader |> when (case model.layout.header of
          Demo.Layout.Waterfall x -> Layout.waterfall x
          Demo.Layout.Seamed -> Layout.seamed
          Demo.Layout.Standard -> Options.nop
          Demo.Layout.Scrolling -> Layout.scrolling)
      , if model.transparentHeader then Layout.transparentHeader else Options.nop
      ]
      { header = header model
      , drawer = if model.layout.withDrawer then drawer else []
      , tabs =
          if model.layout.withTabs then
            (tabTitles, [ Color.background (Color.color model.layout.primary Color.S400) ])
          else
            ([], [])
      , main = [ stylesheet, top ]
      }
    {- ** The following lines are not necessary when you manually set up
       your html, as done with page.html. Removing it will then
       fix the flicker you see on load.
    -}
    |> (\contents ->
      div []
        [ Scheme.topWithScheme model.layout.primary model.layout.accent contents

        , Html.node "script"
           [ Html.Attributes.attribute "src" "https://cdn.polyfill.io/v2/polyfill.js?features=Event.focusin" ]
           []
        , Html.node "script"
           [ Html.Attributes.attribute "src" "assets/highlight/highlight.pack.js" ]
           []
        , case nth model.selectedTab tabs of
            Just ( "Dialog", _, _ ) ->
              Html.map DialogMsg (Demo.Dialog.element model.dialog)
              {- Because of limitations on browsers that have non-native (polyfilled)
              <dialog> elements, our dialog element /may/ have to sit up here. However,
              running in elm-reactor will never load the polyfill, so we render the
              dialog (wrongly if there is no polyfill) only when the Dialog tab is
              active.
              -}
            _ ->
              div [] []
        ]
    )


-- ROUTING


--delta2urlUsingUrlParser : Model -> Model -> Maybe UrlChange
--delta2urlUsingUrlParser _ current =
--  Just <|
--    { entry = NewEntry
--    , url = "#!/" ++ (toString current.counter)
--    }


urlOf : Model -> String
urlOf model =
  "#" ++ (Array.get model.selectedTab tabUrls |> Maybe.withDefault "")


delta2url : Model -> Model -> Maybe Router.UrlChange
delta2url model1 model2 =
  if model1.selectedTab /= model2.selectedTab then
    { entry = Router.NewEntry
    , url = urlOf model2
    } |> Just
  else
    Nothing


location2messages : Navigation.Location -> List Msg
location2messages location =
  [ case String.dropLeft 1 location.hash of
        "" ->
          SelectTab 0

        x ->
          Dict.get x urlTabs
          |> Maybe.withDefault -1
          |> SelectTab
  ]


-- APP


main : Program Never Model Msg
main =
  Navigation.program LocationChanged
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
  ( initModel location, Cmd.batch [ Material.init Mdl, Task.succeed location |> Task.perform LocationChanged ] )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map MenusMsg (Menu.subs Demo.Menus.Mdl model.menus.mdl)
    , Material.subscriptions Mdl model
    ]


-- CSS


stylesheet : Html a
stylesheet =
  Options.stylesheet """
  /* The following line is better done in html. We keep it here for
     compatibility with elm-reactor.
   */
  @import url("assets/highlight/github-gist.css");

  blockquote:before { content: none; }
  blockquote:after { content: none; }
  blockquote {
    border-left-style: solid;
    border-width: 1px;
    padding-left: 1.3ex;
    border-color: rgb(255,82,82);
      /* Really need a way to specify "secondary color" in
         inline css.
       */
    font-style: normal;
  }
  p, blockquote {
    max-width: 40em;
  }

  pre {
    display: inline-block;
    box-sizing: border-box;
    min-width: 100%;
    padding-top: .5rem;
    padding-bottom: 1rem;
    padding-left:1rem;
    margin: 0;
  }
  code {
    font-family: 'Roboto Mono';
  }
  .mdl-layout__header--transparent {
    background: url('https://getmdl.io/assets/demos/transparent.jpg') center / cover;
  }
  .mdl-layout__header--transparent .mdl-layout__drawer-button {
    /* This background is dark, so we set text to white. Use 87% black instead if
       your background is light. */
    color: white;
  }
"""
