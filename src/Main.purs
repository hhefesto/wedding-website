module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State = Unit

data Action = NoOp

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ HP.class_ (HH.ClassName "page") ]
    [ nav
    , hero
    , sectionStory
    , sectionPhotos
    , sectionMenu
    , sectionContact
    , sectionDirections
    ]

nav :: forall m. H.ComponentHTML Action () m
nav =
  HH.header [ HP.class_ (HH.ClassName "nav") ]
    [ HH.div [ HP.class_ (HH.ClassName "nav-logo") ]
        [ HH.span [] [ HH.text "D" ]
        , HH.span [ HP.class_ (HH.ClassName "logo-sep") ] [ HH.text "\xB7" ]
        , HH.span [] [ HH.text "C" ]
        ]
    , HH.nav [ HP.class_ (HH.ClassName "nav-links") ]
        [ HH.a [ HP.href "#story",   HP.class_ (HH.ClassName "nav-link") ] [ HH.text "OUR STORY" ]
        , HH.a [ HP.href "#photos",  HP.class_ (HH.ClassName "nav-link") ] [ HH.text "PHOTOS ALBUM" ]
        , HH.a [ HP.href "#contact", HP.class_ (HH.ClassName "nav-link") ] [ HH.text "CONTACT" ]
        , HH.a [ HP.href "#menu",    HP.class_ (HH.ClassName "nav-link") ] [ HH.text "MENU" ]
        ]
    , HH.a [ HP.href "#directions", HP.class_ (HH.ClassName "nav-direction") ]
        [ HH.text "DIRECTION"
        , HH.span [ HP.class_ (HH.ClassName "arrow") ] [ HH.text " \x2192" ]
        ]
    ]

hero :: forall m. H.ComponentHTML Action () m
hero =
  HH.section [ HP.class_ (HH.ClassName "hero") ]
    [ HH.div [ HP.class_ (HH.ClassName "headline-wrap") ]
        [ HH.h1 [ HP.class_ (HH.ClassName "headline") ]
            [ HH.text "SAVE"
            , HH.br_
            , HH.text "THE"
            , HH.br_
            , HH.text "DATE"
            ]
        , HH.div [ HP.class_ (HH.ClassName "couple-names") ]
            [ HH.text "Daniel & Cristy" ]
        , HH.div [ HP.class_ (HH.ClassName "stamp") ]
            [ stamp ]
        ]
    , HH.div [ HP.class_ (HH.ClassName "cards") ]
        [ photoCard "01." "10 OCT 2026" "SAVE THE DATE"    "./save-the-date.jpeg" true
        , photoCard "02." "DANIEL"      "THE GROOM"        "https://placehold.co/300x480/6a6a6a/ffffff" false
        , photoCard "03." "CRISTY"      "THE BRIDE"        "https://placehold.co/300x480/4a4a4a/ffffff" false
        ]
    ]

stamp :: forall m. H.ComponentHTML Action () m
stamp =
  HH.element (HH.ElemName "svg")
    [ HP.attr (HH.AttrName "viewBox") "0 0 120 120"
    , HP.attr (HH.AttrName "width") "100"
    , HP.attr (HH.AttrName "height") "100"
    , HP.attr (HH.AttrName "xmlns") "http://www.w3.org/2000/svg"
    ]
    [ HH.element (HH.ElemName "defs") []
        [ HH.element (HH.ElemName "path")
            [ HP.id "circle-path"
            , HP.attr (HH.AttrName "d") "M 60,60 m -45,0 a 45,45 0 1,1 90,0 a 45,45 0 1,1 -90,0"
            ]
            []
        ]
    , HH.element (HH.ElemName "text")
        [ HP.attr (HH.AttrName "font-size") "10"
        , HP.attr (HH.AttrName "fill") "#1a1a1a"
        , HP.attr (HH.AttrName "font-family") "Montserrat, sans-serif"
        , HP.attr (HH.AttrName "letter-spacing") "3"
        ]
        [ HH.element (HH.ElemName "textPath")
            [ HP.attr (HH.AttrName "href") "#circle-path" ]
            [ HH.text "\x2736 SAVE THE DATE \x2736 OCT 2026 \x2736 " ]
        ]
    , HH.element (HH.ElemName "line")
        [ HP.attr (HH.AttrName "x1") "60", HP.attr (HH.AttrName "y1") "50"
        , HP.attr (HH.AttrName "x2") "60", HP.attr (HH.AttrName "y2") "46"
        , HP.attr (HH.AttrName "stroke") "#1a1a1a", HP.attr (HH.AttrName "stroke-width") "1"
        ] []
    , HH.element (HH.ElemName "line")
        [ HP.attr (HH.AttrName "x1") "60", HP.attr (HH.AttrName "y1") "74"
        , HP.attr (HH.AttrName "x2") "60", HP.attr (HH.AttrName "y2") "70"
        , HP.attr (HH.AttrName "stroke") "#1a1a1a", HP.attr (HH.AttrName "stroke-width") "1"
        ] []
    , HH.element (HH.ElemName "line")
        [ HP.attr (HH.AttrName "x1") "46", HP.attr (HH.AttrName "y1") "60"
        , HP.attr (HH.AttrName "x2") "50", HP.attr (HH.AttrName "y2") "60"
        , HP.attr (HH.AttrName "stroke") "#1a1a1a", HP.attr (HH.AttrName "stroke-width") "1"
        ] []
    , HH.element (HH.ElemName "line")
        [ HP.attr (HH.AttrName "x1") "74", HP.attr (HH.AttrName "y1") "60"
        , HP.attr (HH.AttrName "x2") "70", HP.attr (HH.AttrName "y2") "60"
        , HP.attr (HH.AttrName "stroke") "#1a1a1a", HP.attr (HH.AttrName "stroke-width") "1"
        ] []
    , HH.element (HH.ElemName "circle")
        [ HP.attr (HH.AttrName "cx") "60", HP.attr (HH.AttrName "cy") "60"
        , HP.attr (HH.AttrName "r") "4"
        , HP.attr (HH.AttrName "fill") "none"
        , HP.attr (HH.AttrName "stroke") "#1a1a1a", HP.attr (HH.AttrName "stroke-width") "1"
        ] []
    ]

photoCard :: forall m. String -> String -> String -> String -> Boolean -> H.ComponentHTML Action () m
photoCard num date title imgSrc featured =
  HH.div [ HP.class_ (HH.ClassName $ "card" <> if featured then " card--featured" else "") ]
    [ HH.div [ HP.class_ (HH.ClassName "card-img-wrap") ]
        [ HH.img
            [ HP.src imgSrc
            , HP.alt title
            , HP.class_ (HH.ClassName $ "card-img" <> if featured then " card-img--illustration" else "")
            ]
        , HH.div [ HP.class_ (HH.ClassName "card-num-wrap") ]
            [ HH.span [ HP.class_ (HH.ClassName "card-num") ] [ HH.text num ] ]
        ]
    , HH.div [ HP.class_ (HH.ClassName "card-info") ]
        [ HH.p [ HP.class_ (HH.ClassName "card-date") ] [ HH.text date ]
        , HH.p [ HP.class_ (HH.ClassName "card-title") ] [ HH.text title ]
        ]
    ]

sectionStory :: forall m. H.ComponentHTML Action () m
sectionStory =
  HH.section [ HP.id "story", HP.class_ (HH.ClassName "section section--story") ]
    [ HH.div [ HP.class_ (HH.ClassName "section-inner") ]
        [ HH.div [ HP.class_ (HH.ClassName "section-label") ] [ HH.text "01" ]
        , HH.h2 [ HP.class_ (HH.ClassName "section-title") ] [ HH.text "Our Story" ]
        , HH.div [ HP.class_ (HH.ClassName "story-body") ]
            [ HH.p [] [ HH.text "What started as a chance encounter grew into a love that changed everything. Daniel and Cristy found in each other not just a partner, but a best friend — someone to laugh with, grow with, and build a life with." ]
            , HH.p [] [ HH.text "After years of adventures together, they are ready to take the next step and celebrate their love surrounded by the people who matter most." ]
            , HH.p [] [ HH.text "Join us on the 10th of October, 2026, as they say \"I do.\"" ]
            ]
        ]
    ]

sectionPhotos :: forall m. H.ComponentHTML Action () m
sectionPhotos =
  HH.section [ HP.id "photos", HP.class_ (HH.ClassName "section section--photos") ]
    [ HH.div [ HP.class_ (HH.ClassName "section-inner") ]
        [ HH.div [ HP.class_ (HH.ClassName "section-label") ] [ HH.text "02" ]
        , HH.h2 [ HP.class_ (HH.ClassName "section-title") ] [ HH.text "Photos Album" ]
        , HH.div [ HP.class_ (HH.ClassName "photo-grid") ]
            [ HH.div [ HP.class_ (HH.ClassName "photo-placeholder") ] [ HH.text "Photo coming soon" ]
            , HH.div [ HP.class_ (HH.ClassName "photo-placeholder") ] [ HH.text "Photo coming soon" ]
            , HH.div [ HP.class_ (HH.ClassName "photo-placeholder") ] [ HH.text "Photo coming soon" ]
            , HH.div [ HP.class_ (HH.ClassName "photo-placeholder") ] [ HH.text "Photo coming soon" ]
            ]
        ]
    ]

sectionMenu :: forall m. H.ComponentHTML Action () m
sectionMenu =
  HH.section [ HP.id "menu", HP.class_ (HH.ClassName "section section--menu") ]
    [ HH.div [ HP.class_ (HH.ClassName "section-inner section-inner--split") ]
        [ HH.div [ HP.class_ (HH.ClassName "section-left") ]
            [ HH.div [ HP.class_ (HH.ClassName "section-label") ] [ HH.text "03" ]
            , HH.h2 [ HP.class_ (HH.ClassName "section-title") ] [ HH.text "Menu" ]
            ]
        , HH.div [ HP.class_ (HH.ClassName "menu-grid") ]
            [ menuCourse "Entrées"
                [ "Garden salad with vinaigrette"
                , "Tomato soup with basil oil"
                ]
            , menuCourse "Main Course"
                [ "Roasted chicken with herbs"
                , "Grilled fish of the season"
                , "Mushroom risotto (v)"
                ]
            , menuCourse "Dessert"
                [ "Wedding cake"
                , "Seasonal fruit tart"
                , "Churros with chocolate"
                ]
            , menuCourse "Drinks"
                [ "Open bar"
                , "Sparkling wine toast"
                , "Soft drinks & water"
                ]
            ]
        ]
    ]

menuCourse :: forall m. String -> Array String -> H.ComponentHTML Action () m
menuCourse course items =
  HH.div [ HP.class_ (HH.ClassName "menu-course") ]
    [ HH.h3 [ HP.class_ (HH.ClassName "menu-course-name") ] [ HH.text course ]
    , HH.ul [ HP.class_ (HH.ClassName "menu-items") ]
        (map (\i -> HH.li [ HP.class_ (HH.ClassName "menu-item") ] [ HH.text i ]) items)
    ]

sectionContact :: forall m. H.ComponentHTML Action () m
sectionContact =
  HH.section [ HP.id "contact", HP.class_ (HH.ClassName "section section--contact") ]
    [ HH.div [ HP.class_ (HH.ClassName "section-inner") ]
        [ HH.div [ HP.class_ (HH.ClassName "section-label") ] [ HH.text "04" ]
        , HH.h2 [ HP.class_ (HH.ClassName "section-title") ] [ HH.text "Contact" ]
        , HH.div [ HP.class_ (HH.ClassName "contact-body") ]
            [ HH.p [ HP.class_ (HH.ClassName "contact-line") ]
                [ HH.span [ HP.class_ (HH.ClassName "contact-label") ] [ HH.text "DATE" ]
                , HH.span [] [ HH.text "10 October 2026" ]
                ]
            , HH.p [ HP.class_ (HH.ClassName "contact-line") ]
                [ HH.span [ HP.class_ (HH.ClassName "contact-label") ] [ HH.text "VENUE" ]
                , HH.span [] [ HH.text "Casa Club Vista Real, Querétaro, México" ]
                ]
            , HH.p [ HP.class_ (HH.ClassName "contact-line") ]
                [ HH.span [ HP.class_ (HH.ClassName "contact-label") ] [ HH.text "RSVP" ]
                , HH.span [] [ HH.text "Please confirm your attendance by 1 September 2026" ]
                ]
            ]
        ]
    ]

sectionDirections :: forall m. H.ComponentHTML Action () m
sectionDirections =
  HH.section [ HP.id "directions", HP.class_ (HH.ClassName "section section--directions") ]
    [ HH.div [ HP.class_ (HH.ClassName "section-inner") ]
        [ HH.div [ HP.class_ (HH.ClassName "section-label") ] [ HH.text "05" ]
        , HH.h2 [ HP.class_ (HH.ClassName "section-title") ] [ HH.text "Direction" ]
        , HH.p [ HP.class_ (HH.ClassName "directions-address") ]
            [ HH.text "Casa Club Vista Real — Querétaro, México" ]
        , HH.div [ HP.class_ (HH.ClassName "map-wrap") ]
            [ HH.element (HH.ElemName "iframe")
                [ HP.attr (HH.AttrName "src") "https://maps.google.com/maps?q=Casa+Club+Vista+Real+Queretaro+Mexico&output=embed&z=16"
                , HP.attr (HH.AttrName "width") "100%"
                , HP.attr (HH.AttrName "height") "100%"
                , HP.attr (HH.AttrName "style") "border:0;"
                , HP.attr (HH.AttrName "allowfullscreen") ""
                , HP.attr (HH.AttrName "loading") "lazy"
                , HP.attr (HH.AttrName "referrerpolicy") "no-referrer-when-downgrade"
                , HP.attr (HH.AttrName "title") "Casa Club Vista Real"
                ]
                []
            ]
        , HH.a
            [ HP.href "https://maps.app.goo.gl/f8TQrUB1Ey4rg5XP6"
            , HP.attr (HH.AttrName "target") "_blank"
            , HP.attr (HH.AttrName "rel") "noopener noreferrer"
            , HP.class_ (HH.ClassName "map-external-link")
            ]
            [ HH.text "Open in Google Maps"
            , HH.span [ HP.class_ (HH.ClassName "arrow") ] [ HH.text " \x2192" ]
            ]
        ]
    ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
