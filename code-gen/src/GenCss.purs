module GenCss where

import MasonPrelude
import Attribute
import Data.Array as Array
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Html (Html)
import Html as H
import Platform as Platform

main :: Effect Unit
main =
  Platform.html
    code

-- view
newtype Property
  = Property (String /\ JoinType)

derive newtype instance eqProperty :: Eq Property

derive newtype instance ordProperty :: Ord Property

data JoinType
  = Comma2
  | Space2
  | Comma1
  | Space1
  | None

derive instance eqJoinType :: Eq JoinType

derive instance ordJoinType :: Ord JoinType

-- view : Html ()
-- view =
--     table []
--         [ thead []
--             [ tr []
--                 [ td [] [ text "Property" ]
--                 , td [] [ text "Join Type" ]
--                 ]
--             ]
--         , tbody [] $
--             List.map toTr $
--                 sort properties
--         ]
-- toTr : Property -> Html ()
-- toTr ( property, joinType ) =
--     tr []
--         [ td [] [ text property ]
--         , td []
--             [ text
--                 (case joinType of
--                     None ->
--                         "None"
--                     Space1 ->
--                         "Space1"
--                     Space2 ->
--                         "Space2"
--                     Comma1 ->
--                         "Comma1"
--                     Comma2 ->
--                         "Comma2"
--                 )
--             ]
--         ]
code ::
  { head :: Array (Html Unit)
  , body :: Array (Html Unit)
  }
code =
  { head: []
  , body:
      [ H.div [] $ map generateCode $ Array.sort properties ]
  }

generateCode :: Property -> Html Unit
generateCode (Property (property /\ joinType)) =
  H.div []
    [ H.pre []
        [ ( """
$ :: String -> Styles
$ = Single <. Declaration Id "#"
"""
              <> ( if joinType == Space1 || joinType == Space2 then
                    j " "
                      <> ( if joinType == Space2 then
                            jj " "
                          else
                            ""
                        )
                  else
                    if joinType == Comma1 then
                      j ", "
                    else
                      if joinType == Comma2 then
                        j " " <> jj ", "
                      else
                        ""
                )
          )
            # String.replaceAll
                (Pattern "$")
                (Replacement $ camelCase property)
            # String.replaceAll
                (Pattern "#")
                (Replacement property)
            # H.text
        ]
    ]

j :: String -> String
j separator =
  String.replaceAll
    (Pattern "%")
    (Replacement separator)
    """
$J :: Array String -> Styles
$J = Single <. Declaration Id "#" <. intercalate "%"
"""

jj :: String -> String
jj separator =
  String.replaceAll
    (Pattern "%")
    (Replacement separator)
    """
$JJ :: Array (Array String) -> Styles
$JJ = Single <. Declaration Id "#" <. intercalate "%" <. map (intercalate " ")
"""

camelCase :: String -> String
camelCase =
  String.split (Pattern "-")
    .> map (somethingFirst String.toUpper)
    .> intercalate ""
    .> somethingFirst String.toLower

somethingFirst :: (String -> String) -> String -> String
somethingFirst func str = case String.uncons str of
  Just { head, tail } ->
    head
      # String.singleton
      # (<>)
      <. func
      ~$ tail
  Nothing -> ""

properties :: Array Property
properties =
  Property
    <$> [ "align-content" /\ Space1
      , "align-items" /\ Space1
      , "align-self" /\ Space1
      , "alignment-baseline" /\ None
      , "all" /\ None
      , "animation" /\ Comma2
      , "animation-delay" /\ Comma1
      , "animation-direction" /\ Comma1
      , "animation-duration" /\ Comma1
      , "animation-fill-mode" /\ Comma1
      , "animation-iteration-count" /\ Comma1
      , "animation-name" /\ Comma1
      , "animation-play-state" /\ Comma1
      , "animation-timing-function" /\ Comma1
      , "appearance" /\ None
      , "azimuth" /\ Space1
      , "backface-visibility" /\ None
      , "background" /\ Comma2
      , "background-attachment" /\ Comma1
      , "background-blend-mode" /\ None
      , "background-clip" /\ Comma1
      , "background-color" /\ None
      , "background-image" /\ Comma1
      , "background-origin" /\ Comma1
      , "background-position" /\ Space1
      , "background-repeat" /\ Comma2
      , "background-size" /\ Comma2
      , "baseline-shift" /\ None
      , "block-overflow" /\ None
      , "block-size" /\ None
      , "block-step" /\ Space1
      , "block-step-align" /\ None
      , "block-step-insert" /\ None
      , "block-step-round" /\ None
      , "block-step-size" /\ None
      , "bookmark-label" /\ Space1
      , "bookmark-level" /\ None
      , "bookmark-state" /\ None
      , "border" /\ Space1
      , "border-block" /\ Space1
      , "border-block-color" /\ Space1
      , "border-block-end" /\ Space1
      , "border-block-end-color" /\ None
      , "border-block-end-style" /\ None
      , "border-block-end-width" /\ None
      , "border-block-start" /\ Space1
      , "border-block-start-color" /\ None
      , "border-block-start-style" /\ None
      , "border-block-start-width" /\ None
      , "border-block-style" /\ Space1
      , "border-block-width" /\ Space1
      , "border-bottom" /\ Space1
      , "border-bottom-color" /\ None
      , "border-bottom-left-radius" /\ Space1
      , "border-bottom-right-radius" /\ Space1
      , "border-bottom-style" /\ None
      , "border-bottom-width" /\ None
      , "border-boundary" /\ None
      , "border-collapse" /\ None
      , "border-color" /\ Space1
      , "border-end-end-radius" /\ Space1
      , "border-end-start-radius" /\ Space1
      , "border-image" /\ Space1
      , "border-image-outset" /\ Space1
      , "border-image-repeat" /\ Space1
      , "border-image-slice" /\ Space1
      , "border-image-source" /\ None
      , "border-image-width" /\ Space1
      , "border-inline" /\ Space1
      , "border-inline-color" /\ Space1
      , "border-inline-end" /\ Space1
      , "border-inline-end-color" /\ None
      , "border-inline-end-style" /\ None
      , "border-inline-end-width" /\ None
      , "border-inline-start" /\ Space1
      , "border-inline-start-color" /\ None
      , "border-inline-start-style" /\ None
      , "border-inline-start-width" /\ None
      , "border-inline-style" /\ Space1
      , "border-inline-width" /\ Space1
      , "border-left" /\ Space1
      , "border-left-color" /\ None
      , "border-left-style" /\ None
      , "border-left-width" /\ None
      , "border-radius" /\ Space1
      , "border-right" /\ Space1
      , "border-right-color" /\ None
      , "border-right-style" /\ None
      , "border-right-width" /\ None
      , "border-spacing" /\ Space1
      , "border-start-end-radius" /\ Space1
      , "border-start-start-radius" /\ Space1
      , "border-style" /\ Space1
      , "border-top" /\ Space1
      , "border-top-color" /\ None
      , "border-top-left-radius" /\ Space1
      , "border-top-right-radius" /\ Space1
      , "border-top-style" /\ None
      , "border-top-width" /\ None
      , "border-width" /\ Space1
      , "bottom" /\ None
      , "box-decoration-break" /\ None
      , "box-shadow" /\ Comma2
      , "box-sizing" /\ None
      , "box-snap" /\ None
      , "break-after" /\ None
      , "break-before" /\ None
      , "break-inside" /\ None
      , "caption-side" /\ None
      , "caret" /\ Space1
      , "caret-color" /\ None
      , "caret-shape" /\ None
      , "clear" /\ None
      , "clip" /\ None
      , "clip-path" /\ Space1
      , "clip-rule" /\ None
      , "color" /\ None
      , "color-adjust" /\ None
      , "color-interpolation-filters" /\ None
      , "color-scheme" /\ Space1
      , "column-count" /\ None
      , "column-fill" /\ None
      , "column-gap" /\ None
      , "column-rule" /\ Space1
      , "column-rule-color" /\ None
      , "column-rule-style" /\ None
      , "column-rule-width" /\ None
      , "column-span" /\ None
      , "column-width" /\ None
      , "columns" /\ Space1
      , "contain" /\ Space1
      , "content" /\ Space1
      , "continue" /\ None
      , "counter-increment" /\ Space2
      , "counter-reset" /\ Space2
      , "counter-set" /\ Space2
      , "cue" /\ Space2
      , "cue-after" /\ Space1
      , "cue-before" /\ Space1
      , "cursor" /\ Space2
      , "direction" /\ None
      , "display" /\ Space1
      , "dominant-baseline" /\ None
      , "elevation" /\ None
      , "empty-cells" /\ None
      , "fill" /\ Space1
      , "fill-break" /\ None
      , "fill-color" /\ None
      , "fill-image" /\ Comma1
      , "fill-opacity" /\ None
      , "fill-origin" /\ None
      , "fill-position" /\ Comma2
      , "fill-repeat" /\ Comma2
      , "fill-rule" /\ None
      , "fill-size" /\ Comma2
      , "filter" /\ Space1
      , "flex" /\ Space1
      , "flex-basis" /\ None
      , "flex-direction" /\ None
      , "flex-flow" /\ Space1
      , "flex-grow" /\ None
      , "flex-shrink" /\ None
      , "flex-wrap" /\ None
      , "float" /\ None
      , "float-defer" /\ None
      , "float-offset" /\ None
      , "float-reference" /\ None
      , "flood-color" /\ None
      , "flood-opacity" /\ None
      , "flow-from" /\ None
      , "flow-into" /\ Space1
      , "font" /\ Space1
      , "font-family" /\ Comma1
      , "font-feature-settings" /\ Comma2
      , "font-kerning" /\ None
      , "font-language-override" /\ None
      , "font-optical-sizing" /\ None
      , "font-palette" /\ None
      , "font-size" /\ None
      , "font-size-adjust" /\ None
      , "font-stretch" /\ None
      , "font-style" /\ Space1
      , "font-synthesis" /\ Space1
      , "font-synthesis-small-caps" /\ None
      , "font-synthesis-style" /\ None
      , "font-synthesis-weight" /\ None
      , "font-variant" /\ Space1
      , "font-variant-alternates" /\ Space1
      , "font-variant-caps" /\ None
      , "font-variant-east-asian" /\ Space1
      , "font-variant-emoji" /\ None
      , "font-variant-ligatures" /\ Space1
      , "font-variant-numeric" /\ Space1
      , "font-variant-position" /\ None
      , "font-variation-settings" /\ Comma2
      , "font-weight" /\ None
      , "footnote-display" /\ None
      , "footnote-policy" /\ None
      , "forced-color-adjust" /\ None
      , "gap" /\ Space1
      , "glyph-orientation-vertical" /\ None
      , "grid" /\ Space2
      , "grid-area" /\ Space2
      , "grid-auto-columns" /\ Space1
      , "grid-auto-flow" /\ Space1
      , "grid-auto-rows" /\ Space1
      , "grid-column" /\ Space2
      , "grid-column-end" /\ Space1
      , "grid-column-start" /\ Space1
      , "grid-row" /\ Space2
      , "grid-row-end" /\ Space1
      , "grid-row-start" /\ Space1
      , "grid-template" /\ Space2
      , "grid-template-areas" /\ Space1
      , "grid-template-columns" /\ Space2
      , "grid-template-rows" /\ Space2
      , "hanging-punctuation" /\ Space1
      , "height" /\ None
      , "hyphenate-character" /\ None
      , "hyphenate-limit-chars" /\ Space1
      , "hyphenate-limit-last" /\ None
      , "hyphenate-limit-lines" /\ None
      , "hyphenate-limit-zone" /\ None
      , "hyphens" /\ None
      , "image-orientation" /\ Space1
      , "image-rendering" /\ None
      , "image-resolution" /\ Space1
      , "initial-letters" /\ Space1
      , "initial-letters-align" /\ Space1
      , "initial-letters-wrap" /\ None
      , "inline-size" /\ None
      , "inline-sizing" /\ None
      , "inset" /\ Space1
      , "inset-block" /\ Space1
      , "inset-block-end" /\ None
      , "inset-block-start" /\ None
      , "inset-inline" /\ Space1
      , "inset-inline-end" /\ None
      , "inset-inline-start" /\ None
      , "isolation" /\ None
      , "justify-content" /\ Space1
      , "justify-items" /\ Space1
      , "justify-self" /\ Space1
      , "left" /\ None
      , "letter-spacing" /\ None
      , "lighting-color" /\ None
      , "line-break" /\ None
      , "line-clamp" /\ Space1
      , "line-grid" /\ None
      , "line-height" /\ None
      , "line-height-step" /\ None
      , "line-padding" /\ None
      , "line-snap" /\ None
      , "list-style" /\ Space1
      , "list-style-image" /\ None
      , "list-style-position" /\ None
      , "list-style-type" /\ None
      , "margin" /\ Space1
      , "margin-block" /\ Space1
      , "margin-block-end" /\ None
      , "margin-block-start" /\ None
      , "margin-bottom" /\ None
      , "margin-break" /\ None
      , "margin-inline" /\ Space1
      , "margin-inline-end" /\ None
      , "margin-inline-start" /\ None
      , "margin-left" /\ None
      , "margin-right" /\ None
      , "margin-top" /\ None
      , "margin-trim" /\ None
      , "marker" /\ Space1
      , "marker-end" /\ None
      , "marker-knockout-left" /\ Space1
      , "marker-knockout-right" /\ Space1
      , "marker-mid" /\ None
      , "marker-pattern" /\ Space2
      , "marker-segment" /\ None
      , "marker-side" /\ None
      , "marker-start" /\ None
      , "mask" /\ Comma2
      , "mask-border" /\ Space2
      , "mask-border-mode" /\ None
      , "mask-border-outset" /\ Space1
      , "mask-border-repeat" /\ Space1
      , "mask-border-slice" /\ Space2
      , "mask-border-source" /\ None
      , "mask-border-width" /\ Space1
      , "mask-clip" /\ Comma1
      , "mask-composite" /\ Comma1
      , "mask-image" /\ Comma1
      , "mask-mode" /\ Comma1
      , "mask-origin" /\ Comma1
      , "mask-position" /\ Comma1
      , "mask-repeat" /\ Comma1
      , "mask-size" /\ Comma1
      , "mask-type" /\ None
      , "max-block-size" /\ None
      , "max-height" /\ None
      , "max-inline-size" /\ None
      , "max-lines" /\ None
      , "max-width" /\ None
      , "min-block-size" /\ None
      , "min-height" /\ None
      , "min-inline-size" /\ None
      , "min-width" /\ None
      , "mix-blend-mode" /\ None
      , "nav-down" /\ Space1
      , "nav-left" /\ Space1
      , "nav-right" /\ Space1
      , "nav-up" /\ Space1
      , "object-fit" /\ None
      , "object-position" /\ Space1
      , "offset" /\ Space2
      , "offset-after" /\ None
      , "offset-anchor" /\ Space1
      , "offset-before" /\ None
      , "offset-distance" /\ None
      , "offset-end" /\ None
      , "offset-path" /\ Space1
      , "offset-position" /\ Space1
      , "offset-rotate" /\ None
      , "offset-start" /\ None
      , "opacity" /\ None
      , "order" /\ None
      , "orphans" /\ None
      , "outline" /\ Space1
      , "outline-color" /\ None
      , "outline-offset" /\ None
      , "outline-style" /\ None
      , "outline-width" /\ None
      , "overflow" /\ Space1
      , "overflow-anchor" /\ None
      , "overflow-block" /\ Space1
      , "overflow-inline" /\ Space1
      , "overflow-wrap" /\ None
      , "overflow-x" /\ None
      , "overflow-y" /\ None
      , "overscroll-behavior" /\ Space1
      , "overscroll-behavior-block" /\ None
      , "overscroll-behavior-inline" /\ None
      , "overscroll-behavior-x" /\ None
      , "overscroll-behavior-y" /\ None
      , "padding" /\ Space1
      , "padding-block" /\ Space1
      , "padding-block-end" /\ None
      , "padding-block-start" /\ None
      , "padding-bottom" /\ None
      , "padding-inline" /\ Space1
      , "padding-inline-end" /\ None
      , "padding-inline-start" /\ None
      , "padding-left" /\ None
      , "padding-right" /\ None
      , "padding-top" /\ None
      , "page" /\ None
      , "page-break-after" /\ None
      , "page-break-before" /\ None
      , "page-break-inside" /\ None
      , "pause" /\ Space1
      , "pause-after" /\ None
      , "pause-before" /\ None
      , "perspective" /\ None
      , "perspective-origin" /\ Space1
      , "pitch" /\ None
      , "pitch-range" /\ None
      , "place-content" /\ Space1
      , "place-items" /\ Space1
      , "place-self" /\ Space1
      , "play-during" /\ Space1
      , "pointer-events" /\ None
      , "position" /\ None
      , "quotes" /\ Space2
      , "region-fragment" /\ None
      , "resize" /\ None
      , "rest" /\ Space1
      , "rest-after" /\ None
      , "rest-before" /\ None
      , "richness" /\ None
      , "right" /\ None
      , "rotate" /\ Space1
      , "row-gap" /\ None
      , "ruby-align" /\ Space1
      , "ruby-merge" /\ None
      , "ruby-position" /\ Space1
      , "running" /\ None
      , "scale" /\ Space1
      , "scroll-behavior" /\ None
      , "scroll-margin" /\ Space1
      , "scroll-margin-block" /\ Space1
      , "scroll-margin-block-end" /\ None
      , "scroll-margin-block-start" /\ None
      , "scroll-margin-bottom" /\ None
      , "scroll-margin-inline" /\ Space1
      , "scroll-margin-inline-end" /\ None
      , "scroll-margin-inline-start" /\ None
      , "scroll-margin-left" /\ None
      , "scroll-margin-right" /\ None
      , "scroll-margin-top" /\ None
      , "scroll-padding" /\ Space1
      , "scroll-padding-block" /\ Space1
      , "scroll-padding-block-end" /\ None
      , "scroll-padding-block-start" /\ None
      , "scroll-padding-bottom" /\ None
      , "scroll-padding-inline" /\ Space1
      , "scroll-padding-inline-end" /\ None
      , "scroll-padding-inline-start" /\ None
      , "scroll-padding-left" /\ None
      , "scroll-padding-right" /\ None
      , "scroll-padding-top" /\ None
      , "scroll-snap-align" /\ Space1
      , "scroll-snap-stop" /\ None
      , "scroll-snap-type" /\ Space1
      , "scrollbar-color" /\ Space1
      , "scrollbar-gutter" /\ Space1
      , "scrollbar-width" /\ None
      , "shape-image-threshold" /\ None
      , "shape-inside" /\ Space1
      , "shape-margin" /\ None
      , "shape-outside" /\ Space1
      , "spatial-navigation-action" /\ None
      , "spatial-navigation-contain" /\ None
      , "spatial-navigation-function" /\ None
      , "speak" /\ None
      , "speak-as" /\ Space1
      , "speak-header" /\ None
      , "speak-numeral" /\ None
      , "speak-punctuation" /\ None
      , "speech-rate" /\ None
      , "stress" /\ None
      , "string-set" /\ Comma2
      , "stroke" /\ Comma2
      , "stroke-align" /\ None
      , "stroke-alignment" /\ None
      , "stroke-break" /\ None
      , "stroke-color" /\ Comma1
      , "stroke-dash-corner" /\ None
      , "stroke-dash-justify" /\ Space1
      , "stroke-dashadjust" /\ Space1
      , "stroke-dasharray" /\ Comma2
      , "stroke-dashcorner" /\ None
      , "stroke-dashoffset" /\ None
      , "stroke-image" /\ Comma1
      , "stroke-linecap" /\ None
      , "stroke-linejoin" /\ Space1
      , "stroke-miterlimit" /\ None
      , "stroke-opacity" /\ None
      , "stroke-origin" /\ None
      , "stroke-position" /\ Comma2
      , "stroke-repeat" /\ Comma2
      , "stroke-size" /\ Comma2
      , "stroke-width" /\ Comma1
      , "tab-size" /\ None
      , "table-layout" /\ None
      , "text-align" /\ None
      , "text-align-all" /\ None
      , "text-align-last" /\ None
      , "text-combine-upright" /\ Space1
      , "text-decoration" /\ Space2
      , "text-decoration-color" /\ None
      , "text-decoration-line" /\ Space1
      , "text-decoration-skip" /\ Space1
      , "text-decoration-skip-ink" /\ None
      , "text-decoration-style" /\ None
      , "text-decoration-width" /\ None
      , "text-emphasis" /\ Space2
      , "text-emphasis-color" /\ None
      , "text-emphasis-position" /\ Space1
      , "text-emphasis-skip" /\ Space1
      , "text-emphasis-style" /\ Space1
      , "text-group-align" /\ None
      , "text-indent" /\ Space1
      , "text-justify" /\ None
      , "text-orientation" /\ None
      , "text-overflow" /\ None
      , "text-shadow" /\ Comma2
      , "text-space-collapse" /\ None
      , "text-space-trim" /\ Space1
      , "text-spacing" /\ Space1
      , "text-transform" /\ Space1
      , "text-underline-offset" /\ None
      , "text-underline-position" /\ Space1
      , "text-wrap" /\ None
      , "top" /\ None
      , "transform" /\ Space1
      , "transform-box" /\ None
      , "transform-origin" /\ Space1
      , "transform-style" /\ None
      , "transition" /\ Comma2
      , "transition-delay" /\ Comma1
      , "transition-duration" /\ Comma1
      , "transition-property" /\ Comma1
      , "transition-timing-function" /\ Comma1
      , "translate" /\ Space1
      , "unicode-bidi" /\ None
      , "user-select" /\ None
      , "vertical-align" /\ Space1
      , "visibility" /\ None
      , "voice-balance" /\ None
      , "voice-duration" /\ None
      , "voice-family" /\ Comma2
      , "voice-pitch" /\ Space1
      , "voice-range" /\ Space1
      , "voice-rate" /\ Space1
      , "voice-stress" /\ None
      , "voice-volume" /\ Space1
      , "volume" /\ None
      , "white-space" /\ None
      , "widows" /\ None
      , "width" /\ None
      , "will-change" /\ Comma1
      , "word-boundary-detection" /\ None
      , "word-boundary-expansion" /\ None
      , "word-break" /\ None
      , "word-spacing" /\ None
      , "word-wrap" /\ None
      , "wrap-after" /\ None
      , "wrap-before" /\ None
      , "wrap-flow" /\ None
      , "wrap-inside" /\ None
      , "wrap-through" /\ None
      , "writing-mode" /\ None
      , "z-index" /\ None
      ]
