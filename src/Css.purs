-- jump to 800
module Css
  ( module VirtualDom.Css
  , append
  , prepend
  , duplicate
  , declaration
  , mapSelector
  , alignContent
  , alignContentJ
  , alignItems
  , alignItemsJ
  , alignSelf
  , alignSelfJ
  , alignmentBaseline
  , all
  , animation
  , animationJ
  , animationJJ
  , animationDelay
  , animationDelayJ
  , animationDirection
  , animationDirectionJ
  , animationDuration
  , animationDurationJ
  , animationFillMode
  , animationFillModeJ
  , animationIterationCount
  , animationIterationCountJ
  , animationName
  , animationNameJ
  , animationPlayState
  , animationPlayStateJ
  , animationTimingFunction
  , animationTimingFunctionJ
  , appearance
  , azimuth
  , azimuthJ
  , backfaceVisibility
  , background
  , backgroundJ
  , backgroundJJ
  , backgroundAttachment
  , backgroundAttachmentJ
  , backgroundBlendMode
  , backgroundClip
  , backgroundClipJ
  , backgroundColor
  , backgroundImage
  , backgroundImageJ
  , backgroundOrigin
  , backgroundOriginJ
  , backgroundPosition
  , backgroundPositionJ
  , backgroundRepeat
  , backgroundRepeatJ
  , backgroundRepeatJJ
  , backgroundSize
  , backgroundSizeJ
  , backgroundSizeJJ
  , baselineShift
  , blockOverflow
  , blockSize
  , blockStep
  , blockStepJ
  , blockStepAlign
  , blockStepInsert
  , blockStepRound
  , blockStepSize
  , bookmarkLabel
  , bookmarkLabelJ
  , bookmarkLevel
  , bookmarkState
  , border
  , borderJ
  , borderBlock
  , borderBlockJ
  , borderBlockColor
  , borderBlockColorJ
  , borderBlockEnd
  , borderBlockEndJ
  , borderBlockEndColor
  , borderBlockEndStyle
  , borderBlockEndWidth
  , borderBlockStart
  , borderBlockStartJ
  , borderBlockStartColor
  , borderBlockStartStyle
  , borderBlockStartWidth
  , borderBlockStyle
  , borderBlockStyleJ
  , borderBlockWidth
  , borderBlockWidthJ
  , borderBottom
  , borderBottomJ
  , borderBottomColor
  , borderBottomLeftRadius
  , borderBottomLeftRadiusJ
  , borderBottomRightRadius
  , borderBottomRightRadiusJ
  , borderBottomStyle
  , borderBottomWidth
  , borderBoundary
  , borderCollapse
  , borderColor
  , borderColorJ
  , borderEndEndRadius
  , borderEndEndRadiusJ
  , borderEndStartRadius
  , borderEndStartRadiusJ
  , borderImage
  , borderImageJ
  , borderImageOutset
  , borderImageOutsetJ
  , borderImageRepeat
  , borderImageRepeatJ
  , borderImageSlice
  , borderImageSliceJ
  , borderImageSource
  , borderImageWidth
  , borderImageWidthJ
  , borderInline
  , borderInlineJ
  , borderInlineColor
  , borderInlineColorJ
  , borderInlineEnd
  , borderInlineEndJ
  , borderInlineEndColor
  , borderInlineEndStyle
  , borderInlineEndWidth
  , borderInlineStart
  , borderInlineStartJ
  , borderInlineStartColor
  , borderInlineStartStyle
  , borderInlineStartWidth
  , borderInlineStyle
  , borderInlineStyleJ
  , borderInlineWidth
  , borderInlineWidthJ
  , borderLeft
  , borderLeftJ
  , borderLeftColor
  , borderLeftStyle
  , borderLeftWidth
  , borderRadius
  , borderRadiusJ
  , borderRight
  , borderRightJ
  , borderRightColor
  , borderRightStyle
  , borderRightWidth
  , borderSpacing
  , borderSpacingJ
  , borderStartEndRadius
  , borderStartEndRadiusJ
  , borderStartStartRadius
  , borderStartStartRadiusJ
  , borderStyle
  , borderStyleJ
  , borderTop
  , borderTopJ
  , borderTopColor
  , borderTopLeftRadius
  , borderTopLeftRadiusJ
  , borderTopRightRadius
  , borderTopRightRadiusJ
  , borderTopStyle
  , borderTopWidth
  , borderWidth
  , borderWidthJ
  , bottom
  , boxDecorationBreak
  , boxShadow
  , boxShadowJ
  , boxShadowJJ
  , boxSizing
  , boxSnap
  , breakAfter
  , breakBefore
  , breakInside
  , captionSide
  , caret
  , caretJ
  , caretColor
  , caretShape
  , clear
  , clip
  , clipPath
  , clipPathJ
  , clipRule
  , color
  , colorAdjust
  , colorInterpolationFilters
  , colorScheme
  , colorSchemeJ
  , columnCount
  , columnFill
  , columnGap
  , columnRule
  , columnRuleJ
  , columnRuleColor
  , columnRuleStyle
  , columnRuleWidth
  , columnSpan
  , columnWidth
  , columns
  , columnsJ
  , contain
  , containJ
  , content
  , contentJ
  , continue
  , counterIncrement
  , counterIncrementJ
  , counterIncrementJJ
  , counterReset
  , counterResetJ
  , counterResetJJ
  , counterSet
  , counterSetJ
  , counterSetJJ
  , cue
  , cueJ
  , cueJJ
  , cueAfter
  , cueAfterJ
  , cueBefore
  , cueBeforeJ
  , cursor
  , cursorJ
  , cursorJJ
  , direction
  , display
  , displayJ
  , dominantBaseline
  , elevation
  , emptyCells
  , fill
  , fillJ
  , fillBreak
  , fillColor
  , fillImage
  , fillImageJ
  , fillOpacity
  , fillOrigin
  , fillPosition
  , fillPositionJ
  , fillPositionJJ
  , fillRepeat
  , fillRepeatJ
  , fillRepeatJJ
  , fillRule
  , fillSize
  , fillSizeJ
  , fillSizeJJ
  , filter
  , filterJ
  , flex
  , flexJ
  , flexBasis
  , flexDirection
  , flexFlow
  , flexFlowJ
  , flexGrow
  , flexShrink
  , flexWrap
  , float
  , floatDefer
  , floatOffset
  , floatReference
  , floodColor
  , floodOpacity
  , flowFrom
  , flowInto
  , flowIntoJ
  , font
  , fontJ
  , fontFamily
  , fontFamilyJ
  , fontFeatureSettings
  , fontFeatureSettingsJ
  , fontFeatureSettingsJJ
  , fontKerning
  , fontLanguageOverride
  , fontOpticalSizing
  , fontPalette
  , fontSize
  , fontSizeAdjust
  , fontStretch
  , fontStyle
  , fontStyleJ
  , fontSynthesis
  , fontSynthesisJ
  , fontSynthesisSmallCaps
  , fontSynthesisStyle
  , fontSynthesisWeight
  , fontVariant
  , fontVariantJ
  , fontVariantAlternates
  , fontVariantAlternatesJ
  , fontVariantCaps
  , fontVariantEastAsian
  , fontVariantEastAsianJ
  , fontVariantEmoji
  , fontVariantLigatures
  , fontVariantLigaturesJ
  , fontVariantNumeric
  , fontVariantNumericJ
  , fontVariantPosition
  , fontVariationSettings
  , fontVariationSettingsJ
  , fontVariationSettingsJJ
  , fontWeight
  , footnoteDisplay
  , footnotePolicy
  , forcedColorAdjust
  , gap
  , gapJ
  , glyphOrientationVertical
  , grid
  , gridJ
  , gridJJ
  , gridArea
  , gridAreaJ
  , gridAreaJJ
  , gridAutoColumns
  , gridAutoColumnsJ
  , gridAutoFlow
  , gridAutoFlowJ
  , gridAutoRows
  , gridAutoRowsJ
  , gridColumn
  , gridColumnJ
  , gridColumnJJ
  , gridColumnEnd
  , gridColumnEndJ
  , gridColumnStart
  , gridColumnStartJ
  , gridRow
  , gridRowJ
  , gridRowJJ
  , gridRowEnd
  , gridRowEndJ
  , gridRowStart
  , gridRowStartJ
  , gridTemplate
  , gridTemplateJ
  , gridTemplateJJ
  , gridTemplateAreas
  , gridTemplateAreasJ
  , gridTemplateColumns
  , gridTemplateColumnsJ
  , gridTemplateColumnsJJ
  , gridTemplateRows
  , gridTemplateRowsJ
  , gridTemplateRowsJJ
  , hangingPunctuation
  , hangingPunctuationJ
  , height
  , hyphenateCharacter
  , hyphenateLimitChars
  , hyphenateLimitCharsJ
  , hyphenateLimitLast
  , hyphenateLimitLines
  , hyphenateLimitZone
  , hyphens
  , imageOrientation
  , imageOrientationJ
  , imageRendering
  , imageResolution
  , imageResolutionJ
  , initialLetters
  , initialLettersJ
  , initialLettersAlign
  , initialLettersAlignJ
  , initialLettersWrap
  , inlineSize
  , inlineSizing
  , inset
  , insetJ
  , insetBlock
  , insetBlockJ
  , insetBlockEnd
  , insetBlockStart
  , insetInline
  , insetInlineJ
  , insetInlineEnd
  , insetInlineStart
  , isolation
  , justifyContent
  , justifyContentJ
  , justifyItems
  , justifyItemsJ
  , justifySelf
  , justifySelfJ
  , left
  , letterSpacing
  , lightingColor
  , lineBreak
  , lineClamp
  , lineClampJ
  , lineGrid
  , lineHeight
  , lineHeightStep
  , linePadding
  , lineSnap
  , listStyle
  , listStyleJ
  , listStyleImage
  , listStylePosition
  , listStyleType
  , margin
  , marginJ
  , marginBlock
  , marginBlockJ
  , marginBlockEnd
  , marginBlockStart
  , marginBottom
  , marginBreak
  , marginInline
  , marginInlineJ
  , marginInlineEnd
  , marginInlineStart
  , marginLeft
  , marginRight
  , marginTop
  , marginTrim
  , marker
  , markerJ
  , markerEnd
  , markerKnockoutLeft
  , markerKnockoutLeftJ
  , markerKnockoutRight
  , markerKnockoutRightJ
  , markerMid
  , markerPattern
  , markerPatternJ
  , markerPatternJJ
  , markerSegment
  , markerSide
  , markerStart
  , mask
  , maskJ
  , maskJJ
  , maskBorder
  , maskBorderJ
  , maskBorderJJ
  , maskBorderMode
  , maskBorderOutset
  , maskBorderOutsetJ
  , maskBorderRepeat
  , maskBorderRepeatJ
  , maskBorderSlice
  , maskBorderSliceJ
  , maskBorderSliceJJ
  , maskBorderSource
  , maskBorderWidth
  , maskBorderWidthJ
  , maskClip
  , maskClipJ
  , maskComposite
  , maskCompositeJ
  , maskImage
  , maskImageJ
  , maskMode
  , maskModeJ
  , maskOrigin
  , maskOriginJ
  , maskPosition
  , maskPositionJ
  , maskRepeat
  , maskRepeatJ
  , maskSize
  , maskSizeJ
  , maskType
  , maxBlockSize
  , maxHeight
  , maxInlineSize
  , maxLines
  , maxWidth
  , minBlockSize
  , minHeight
  , minInlineSize
  , minWidth
  , mixBlendMode
  , navDown
  , navDownJ
  , navLeft
  , navLeftJ
  , navRight
  , navRightJ
  , navUp
  , navUpJ
  , objectFit
  , objectPosition
  , objectPositionJ
  , offset
  , offsetJ
  , offsetJJ
  , offsetAfter
  , offsetAnchor
  , offsetAnchorJ
  , offsetBefore
  , offsetDistance
  , offsetEnd
  , offsetPath
  , offsetPathJ
  , offsetPosition
  , offsetPositionJ
  , offsetRotate
  , offsetStart
  , opacity
  , order
  , orphans
  , outline
  , outlineJ
  , outlineColor
  , outlineOffset
  , outlineStyle
  , outlineWidth
  , overflow
  , overflowJ
  , overflowAnchor
  , overflowBlock
  , overflowBlockJ
  , overflowInline
  , overflowInlineJ
  , overflowWrap
  , overflowX
  , overflowY
  , overscrollBehavior
  , overscrollBehaviorJ
  , overscrollBehaviorBlock
  , overscrollBehaviorInline
  , overscrollBehaviorX
  , overscrollBehaviorY
  , padding
  , paddingJ
  , paddingBlock
  , paddingBlockJ
  , paddingBlockEnd
  , paddingBlockStart
  , paddingBottom
  , paddingInline
  , paddingInlineJ
  , paddingInlineEnd
  , paddingInlineStart
  , paddingLeft
  , paddingRight
  , paddingTop
  , page
  , pageBreakAfter
  , pageBreakBefore
  , pageBreakInside
  , pause
  , pauseJ
  , pauseAfter
  , pauseBefore
  , perspective
  , perspectiveOrigin
  , perspectiveOriginJ
  , pitch
  , pitchRange
  , placeContent
  , placeContentJ
  , placeItems
  , placeItemsJ
  , placeSelf
  , placeSelfJ
  , playDuring
  , playDuringJ
  , pointerEvents
  , position
  , quotes
  , quotesJ
  , quotesJJ
  , regionFragment
  , resize
  , rest
  , restJ
  , restAfter
  , restBefore
  , richness
  , right
  , rotate
  , rotateJ
  , rowGap
  , rubyAlign
  , rubyAlignJ
  , rubyMerge
  , rubyPosition
  , rubyPositionJ
  , running
  , scale
  , scaleJ
  , scrollBehavior
  , scrollMargin
  , scrollMarginJ
  , scrollMarginBlock
  , scrollMarginBlockJ
  , scrollMarginBlockEnd
  , scrollMarginBlockStart
  , scrollMarginBottom
  , scrollMarginInline
  , scrollMarginInlineJ
  , scrollMarginInlineEnd
  , scrollMarginInlineStart
  , scrollMarginLeft
  , scrollMarginRight
  , scrollMarginTop
  , scrollPadding
  , scrollPaddingJ
  , scrollPaddingBlock
  , scrollPaddingBlockJ
  , scrollPaddingBlockEnd
  , scrollPaddingBlockStart
  , scrollPaddingBottom
  , scrollPaddingInline
  , scrollPaddingInlineJ
  , scrollPaddingInlineEnd
  , scrollPaddingInlineStart
  , scrollPaddingLeft
  , scrollPaddingRight
  , scrollPaddingTop
  , scrollSnapAlign
  , scrollSnapAlignJ
  , scrollSnapStop
  , scrollSnapType
  , scrollSnapTypeJ
  , scrollbarColor
  , scrollbarColorJ
  , scrollbarGutter
  , scrollbarGutterJ
  , scrollbarWidth
  , shapeImageThreshold
  , shapeInside
  , shapeInsideJ
  , shapeMargin
  , shapeOutside
  , shapeOutsideJ
  , spatialNavigationAction
  , spatialNavigationContain
  , spatialNavigationFunction
  , speak
  , speakAs
  , speakAsJ
  , speakHeader
  , speakNumeral
  , speakPunctuation
  , speechRate
  , stress
  , stringSet
  , stringSetJ
  , stringSetJJ
  , stroke
  , strokeJ
  , strokeJJ
  , strokeAlign
  , strokeAlignment
  , strokeBreak
  , strokeColor
  , strokeColorJ
  , strokeDashCorner
  , strokeDashJustify
  , strokeDashJustifyJ
  , strokeDashadjust
  , strokeDashadjustJ
  , strokeDasharray
  , strokeDasharrayJ
  , strokeDasharrayJJ
  , strokeDashcorner
  , strokeDashoffset
  , strokeImage
  , strokeImageJ
  , strokeLinecap
  , strokeLinejoin
  , strokeLinejoinJ
  , strokeMiterlimit
  , strokeOpacity
  , strokeOrigin
  , strokePosition
  , strokePositionJ
  , strokePositionJJ
  , strokeRepeat
  , strokeRepeatJ
  , strokeRepeatJJ
  , strokeSize
  , strokeSizeJ
  , strokeSizeJJ
  , strokeWidth
  , strokeWidthJ
  , tabSize
  , tableLayout
  , textAlign
  , textAlignAll
  , textAlignLast
  , textCombineUpright
  , textCombineUprightJ
  , textDecoration
  , textDecorationJ
  , textDecorationJJ
  , textDecorationColor
  , textDecorationLine
  , textDecorationLineJ
  , textDecorationSkip
  , textDecorationSkipJ
  , textDecorationSkipInk
  , textDecorationStyle
  , textDecorationWidth
  , textEmphasis
  , textEmphasisJ
  , textEmphasisJJ
  , textEmphasisColor
  , textEmphasisPosition
  , textEmphasisPositionJ
  , textEmphasisSkip
  , textEmphasisSkipJ
  , textEmphasisStyle
  , textEmphasisStyleJ
  , textGroupAlign
  , textIndent
  , textIndentJ
  , textJustify
  , textOrientation
  , textOverflow
  , textShadow
  , textShadowJ
  , textShadowJJ
  , textSpaceCollapse
  , textSpaceTrim
  , textSpaceTrimJ
  , textSpacing
  , textSpacingJ
  , textTransform
  , textTransformJ
  , textUnderlineOffset
  , textUnderlinePosition
  , textUnderlinePositionJ
  , textWrap
  , top
  , transform
  , transformJ
  , transformBox
  , transformOrigin
  , transformOriginJ
  , transformStyle
  , transition
  , transitionJ
  , transitionJJ
  , transitionDelay
  , transitionDelayJ
  , transitionDuration
  , transitionDurationJ
  , transitionProperty
  , transitionPropertyJ
  , transitionTimingFunction
  , transitionTimingFunctionJ
  , translate
  , translateJ
  , unicodeBidi
  , userSelect
  , verticalAlign
  , verticalAlignJ
  , visibility
  , voiceBalance
  , voiceDuration
  , voiceFamily
  , voiceFamilyJ
  , voiceFamilyJJ
  , voicePitch
  , voicePitchJ
  , voiceRange
  , voiceRangeJ
  , voiceRate
  , voiceRateJ
  , voiceStress
  , voiceVolume
  , voiceVolumeJ
  , volume
  , whiteSpace
  , widows
  , width
  , willChange
  , willChangeJ
  , wordBoundaryDetection
  , wordBoundaryExpansion
  , wordBreak
  , wordSpacing
  , wordWrap
  , wrapAfter
  , wrapBefore
  , wrapFlow
  , wrapInside
  , wrapThrough
  , writingMode
  , zIndex
  , pct
  , em
  , ex
  , ch
  , rem
  , vw
  , vh
  , vmin
  , vmax
  , cm
  , mm
  , q
  , in_
  , pc
  , pt
  , px
  , deg
  , grad
  , rad
  , turn
  , s
  , ms
  , hz
  , kHz
  , dpi
  , dpcm
  , dppx
  , fr
  ) where

import MasonPrelude
import Data.Array as Array
import Data.Batchable (Batched(..), flattenMap)
import VirtualDom.Css (Style(..), Styles, StringOp(..))

append :: String -> StringOp
append str = Id <> Const str

prepend :: String -> StringOp
prepend str = Const str <> Id

duplicate :: String -> StringOp
duplicate str = Id <> Const str <> Id

declaration :: String -> String -> Styles
declaration = Single <.. Declaration Id

mapSelector :: StringOp -> Array Styles -> Styles
mapSelector op styles =
  Batch styles
    # flattenMap
        ( \(Declaration op' p v) ->
            Single $ Declaration (Compose op op') p v
        )
    # Array.fromFoldable
    # Batch

-- TODO: add generated classes
alignContent :: String -> Styles
alignContent = declaration "align-content"

alignContentJ :: Array String -> Styles
alignContentJ = declaration "align-content" <. intercalate " "

alignItems :: String -> Styles
alignItems = declaration "align-items"

alignItemsJ :: Array String -> Styles
alignItemsJ = declaration "align-items" <. intercalate " "

alignSelf :: String -> Styles
alignSelf = declaration "align-self"

alignSelfJ :: Array String -> Styles
alignSelfJ = declaration "align-self" <. intercalate " "

alignmentBaseline :: String -> Styles
alignmentBaseline = declaration "alignment-baseline"

all :: String -> Styles
all = declaration "all"

animation :: String -> Styles
animation = declaration "animation"

animationJ :: Array String -> Styles
animationJ = declaration "animation" <. intercalate " "

animationJJ :: Array (Array String) -> Styles
animationJJ = declaration "animation" <. intercalate ", " <. map (intercalate " ")

animationDelay :: String -> Styles
animationDelay = declaration "animation-delay"

animationDelayJ :: Array String -> Styles
animationDelayJ = declaration "animation-delay" <. intercalate ", "

animationDirection :: String -> Styles
animationDirection = declaration "animation-direction"

animationDirectionJ :: Array String -> Styles
animationDirectionJ = declaration "animation-direction" <. intercalate ", "

animationDuration :: String -> Styles
animationDuration = declaration "animation-duration"

animationDurationJ :: Array String -> Styles
animationDurationJ = declaration "animation-duration" <. intercalate ", "

animationFillMode :: String -> Styles
animationFillMode = declaration "animation-fill-mode"

animationFillModeJ :: Array String -> Styles
animationFillModeJ = declaration "animation-fill-mode" <. intercalate ", "

animationIterationCount :: String -> Styles
animationIterationCount = declaration "animation-iteration-count"

animationIterationCountJ :: Array String -> Styles
animationIterationCountJ = declaration "animation-iteration-count" <. intercalate ", "

animationName :: String -> Styles
animationName = declaration "animation-name"

animationNameJ :: Array String -> Styles
animationNameJ = declaration "animation-name" <. intercalate ", "

animationPlayState :: String -> Styles
animationPlayState = declaration "animation-play-state"

animationPlayStateJ :: Array String -> Styles
animationPlayStateJ = declaration "animation-play-state" <. intercalate ", "

animationTimingFunction :: String -> Styles
animationTimingFunction = declaration "animation-timing-function"

animationTimingFunctionJ :: Array String -> Styles
animationTimingFunctionJ = declaration "animation-timing-function" <. intercalate ", "

appearance :: String -> Styles
appearance = declaration "appearance"

azimuth :: String -> Styles
azimuth = declaration "azimuth"

azimuthJ :: Array String -> Styles
azimuthJ = declaration "azimuth" <. intercalate " "

backfaceVisibility :: String -> Styles
backfaceVisibility = declaration "backface-visibility"

background :: String -> Styles
background = declaration "background"

backgroundJ :: Array String -> Styles
backgroundJ = declaration "background" <. intercalate " "

backgroundJJ :: Array (Array String) -> Styles
backgroundJJ = declaration "background" <. intercalate ", " <. map (intercalate " ")

backgroundAttachment :: String -> Styles
backgroundAttachment = declaration "background-attachment"

backgroundAttachmentJ :: Array String -> Styles
backgroundAttachmentJ = declaration "background-attachment" <. intercalate ", "

backgroundBlendMode :: String -> Styles
backgroundBlendMode = declaration "background-blend-mode"

backgroundClip :: String -> Styles
backgroundClip = declaration "background-clip"

backgroundClipJ :: Array String -> Styles
backgroundClipJ = declaration "background-clip" <. intercalate ", "

backgroundColor :: String -> Styles
backgroundColor = declaration "background-color"

backgroundImage :: String -> Styles
backgroundImage = declaration "background-image"

backgroundImageJ :: Array String -> Styles
backgroundImageJ = declaration "background-image" <. intercalate ", "

backgroundOrigin :: String -> Styles
backgroundOrigin = declaration "background-origin"

backgroundOriginJ :: Array String -> Styles
backgroundOriginJ = declaration "background-origin" <. intercalate ", "

backgroundPosition :: String -> Styles
backgroundPosition = declaration "background-position"

backgroundPositionJ :: Array String -> Styles
backgroundPositionJ = declaration "background-position" <. intercalate " "

backgroundRepeat :: String -> Styles
backgroundRepeat = declaration "background-repeat"

backgroundRepeatJ :: Array String -> Styles
backgroundRepeatJ = declaration "background-repeat" <. intercalate " "

backgroundRepeatJJ :: Array (Array String) -> Styles
backgroundRepeatJJ = declaration "background-repeat" <. intercalate ", " <. map (intercalate " ")

backgroundSize :: String -> Styles
backgroundSize = declaration "background-size"

backgroundSizeJ :: Array String -> Styles
backgroundSizeJ = declaration "background-size" <. intercalate " "

backgroundSizeJJ :: Array (Array String) -> Styles
backgroundSizeJJ = declaration "background-size" <. intercalate ", " <. map (intercalate " ")

baselineShift :: String -> Styles
baselineShift = declaration "baseline-shift"

blockOverflow :: String -> Styles
blockOverflow = declaration "block-overflow"

blockSize :: String -> Styles
blockSize = declaration "block-size"

blockStep :: String -> Styles
blockStep = declaration "block-step"

blockStepJ :: Array String -> Styles
blockStepJ = declaration "block-step" <. intercalate " "

blockStepAlign :: String -> Styles
blockStepAlign = declaration "block-step-align"

blockStepInsert :: String -> Styles
blockStepInsert = declaration "block-step-insert"

blockStepRound :: String -> Styles
blockStepRound = declaration "block-step-round"

blockStepSize :: String -> Styles
blockStepSize = declaration "block-step-size"

bookmarkLabel :: String -> Styles
bookmarkLabel = declaration "bookmark-label"

bookmarkLabelJ :: Array String -> Styles
bookmarkLabelJ = declaration "bookmark-label" <. intercalate " "

bookmarkLevel :: String -> Styles
bookmarkLevel = declaration "bookmark-level"

bookmarkState :: String -> Styles
bookmarkState = declaration "bookmark-state"

border :: String -> Styles
border = declaration "border"

borderJ :: Array String -> Styles
borderJ = declaration "border" <. intercalate " "

borderBlock :: String -> Styles
borderBlock = declaration "border-block"

borderBlockJ :: Array String -> Styles
borderBlockJ = declaration "border-block" <. intercalate " "

borderBlockColor :: String -> Styles
borderBlockColor = declaration "border-block-color"

borderBlockColorJ :: Array String -> Styles
borderBlockColorJ = declaration "border-block-color" <. intercalate " "

borderBlockEnd :: String -> Styles
borderBlockEnd = declaration "border-block-end"

borderBlockEndJ :: Array String -> Styles
borderBlockEndJ = declaration "border-block-end" <. intercalate " "

borderBlockEndColor :: String -> Styles
borderBlockEndColor = declaration "border-block-end-color"

borderBlockEndStyle :: String -> Styles
borderBlockEndStyle = declaration "border-block-end-style"

borderBlockEndWidth :: String -> Styles
borderBlockEndWidth = declaration "border-block-end-width"

borderBlockStart :: String -> Styles
borderBlockStart = declaration "border-block-start"

borderBlockStartJ :: Array String -> Styles
borderBlockStartJ = declaration "border-block-start" <. intercalate " "

borderBlockStartColor :: String -> Styles
borderBlockStartColor = declaration "border-block-start-color"

borderBlockStartStyle :: String -> Styles
borderBlockStartStyle = declaration "border-block-start-style"

borderBlockStartWidth :: String -> Styles
borderBlockStartWidth = declaration "border-block-start-width"

borderBlockStyle :: String -> Styles
borderBlockStyle = declaration "border-block-style"

borderBlockStyleJ :: Array String -> Styles
borderBlockStyleJ = declaration "border-block-style" <. intercalate " "

borderBlockWidth :: String -> Styles
borderBlockWidth = declaration "border-block-width"

borderBlockWidthJ :: Array String -> Styles
borderBlockWidthJ = declaration "border-block-width" <. intercalate " "

borderBottom :: String -> Styles
borderBottom = declaration "border-bottom"

borderBottomJ :: Array String -> Styles
borderBottomJ = declaration "border-bottom" <. intercalate " "

borderBottomColor :: String -> Styles
borderBottomColor = declaration "border-bottom-color"

borderBottomLeftRadius :: String -> Styles
borderBottomLeftRadius = declaration "border-bottom-left-radius"

borderBottomLeftRadiusJ :: Array String -> Styles
borderBottomLeftRadiusJ = declaration "border-bottom-left-radius" <. intercalate " "

borderBottomRightRadius :: String -> Styles
borderBottomRightRadius = declaration "border-bottom-right-radius"

borderBottomRightRadiusJ :: Array String -> Styles
borderBottomRightRadiusJ = declaration "border-bottom-right-radius" <. intercalate " "

borderBottomStyle :: String -> Styles
borderBottomStyle = declaration "border-bottom-style"

borderBottomWidth :: String -> Styles
borderBottomWidth = declaration "border-bottom-width"

borderBoundary :: String -> Styles
borderBoundary = declaration "border-boundary"

borderCollapse :: String -> Styles
borderCollapse = declaration "border-collapse"

borderColor :: String -> Styles
borderColor = declaration "border-color"

borderColorJ :: Array String -> Styles
borderColorJ = declaration "border-color" <. intercalate " "

borderEndEndRadius :: String -> Styles
borderEndEndRadius = declaration "border-end-end-radius"

borderEndEndRadiusJ :: Array String -> Styles
borderEndEndRadiusJ = declaration "border-end-end-radius" <. intercalate " "

borderEndStartRadius :: String -> Styles
borderEndStartRadius = declaration "border-end-start-radius"

borderEndStartRadiusJ :: Array String -> Styles
borderEndStartRadiusJ = declaration "border-end-start-radius" <. intercalate " "

borderImage :: String -> Styles
borderImage = declaration "border-image"

borderImageJ :: Array String -> Styles
borderImageJ = declaration "border-image" <. intercalate " "

borderImageOutset :: String -> Styles
borderImageOutset = declaration "border-image-outset"

borderImageOutsetJ :: Array String -> Styles
borderImageOutsetJ = declaration "border-image-outset" <. intercalate " "

borderImageRepeat :: String -> Styles
borderImageRepeat = declaration "border-image-repeat"

borderImageRepeatJ :: Array String -> Styles
borderImageRepeatJ = declaration "border-image-repeat" <. intercalate " "

borderImageSlice :: String -> Styles
borderImageSlice = declaration "border-image-slice"

borderImageSliceJ :: Array String -> Styles
borderImageSliceJ = declaration "border-image-slice" <. intercalate " "

borderImageSource :: String -> Styles
borderImageSource = declaration "border-image-source"

borderImageWidth :: String -> Styles
borderImageWidth = declaration "border-image-width"

borderImageWidthJ :: Array String -> Styles
borderImageWidthJ = declaration "border-image-width" <. intercalate " "

borderInline :: String -> Styles
borderInline = declaration "border-inline"

borderInlineJ :: Array String -> Styles
borderInlineJ = declaration "border-inline" <. intercalate " "

borderInlineColor :: String -> Styles
borderInlineColor = declaration "border-inline-color"

borderInlineColorJ :: Array String -> Styles
borderInlineColorJ = declaration "border-inline-color" <. intercalate " "

borderInlineEnd :: String -> Styles
borderInlineEnd = declaration "border-inline-end"

borderInlineEndJ :: Array String -> Styles
borderInlineEndJ = declaration "border-inline-end" <. intercalate " "

borderInlineEndColor :: String -> Styles
borderInlineEndColor = declaration "border-inline-end-color"

borderInlineEndStyle :: String -> Styles
borderInlineEndStyle = declaration "border-inline-end-style"

borderInlineEndWidth :: String -> Styles
borderInlineEndWidth = declaration "border-inline-end-width"

borderInlineStart :: String -> Styles
borderInlineStart = declaration "border-inline-start"

borderInlineStartJ :: Array String -> Styles
borderInlineStartJ = declaration "border-inline-start" <. intercalate " "

borderInlineStartColor :: String -> Styles
borderInlineStartColor = declaration "border-inline-start-color"

borderInlineStartStyle :: String -> Styles
borderInlineStartStyle = declaration "border-inline-start-style"

borderInlineStartWidth :: String -> Styles
borderInlineStartWidth = declaration "border-inline-start-width"

borderInlineStyle :: String -> Styles
borderInlineStyle = declaration "border-inline-style"

borderInlineStyleJ :: Array String -> Styles
borderInlineStyleJ = declaration "border-inline-style" <. intercalate " "

borderInlineWidth :: String -> Styles
borderInlineWidth = declaration "border-inline-width"

borderInlineWidthJ :: Array String -> Styles
borderInlineWidthJ = declaration "border-inline-width" <. intercalate " "

borderLeft :: String -> Styles
borderLeft = declaration "border-left"

borderLeftJ :: Array String -> Styles
borderLeftJ = declaration "border-left" <. intercalate " "

borderLeftColor :: String -> Styles
borderLeftColor = declaration "border-left-color"

borderLeftStyle :: String -> Styles
borderLeftStyle = declaration "border-left-style"

borderLeftWidth :: String -> Styles
borderLeftWidth = declaration "border-left-width"

borderRadius :: String -> Styles
borderRadius = declaration "border-radius"

borderRadiusJ :: Array String -> Styles
borderRadiusJ = declaration "border-radius" <. intercalate " "

borderRight :: String -> Styles
borderRight = declaration "border-right"

borderRightJ :: Array String -> Styles
borderRightJ = declaration "border-right" <. intercalate " "

borderRightColor :: String -> Styles
borderRightColor = declaration "border-right-color"

borderRightStyle :: String -> Styles
borderRightStyle = declaration "border-right-style"

borderRightWidth :: String -> Styles
borderRightWidth = declaration "border-right-width"

borderSpacing :: String -> Styles
borderSpacing = declaration "border-spacing"

borderSpacingJ :: Array String -> Styles
borderSpacingJ = declaration "border-spacing" <. intercalate " "

borderStartEndRadius :: String -> Styles
borderStartEndRadius = declaration "border-start-end-radius"

borderStartEndRadiusJ :: Array String -> Styles
borderStartEndRadiusJ = declaration "border-start-end-radius" <. intercalate " "

borderStartStartRadius :: String -> Styles
borderStartStartRadius = declaration "border-start-start-radius"

borderStartStartRadiusJ :: Array String -> Styles
borderStartStartRadiusJ = declaration "border-start-start-radius" <. intercalate " "

borderStyle :: String -> Styles
borderStyle = declaration "border-style"

borderStyleJ :: Array String -> Styles
borderStyleJ = declaration "border-style" <. intercalate " "

borderTop :: String -> Styles
borderTop = declaration "border-top"

borderTopJ :: Array String -> Styles
borderTopJ = declaration "border-top" <. intercalate " "

borderTopColor :: String -> Styles
borderTopColor = declaration "border-top-color"

borderTopLeftRadius :: String -> Styles
borderTopLeftRadius = declaration "border-top-left-radius"

borderTopLeftRadiusJ :: Array String -> Styles
borderTopLeftRadiusJ = declaration "border-top-left-radius" <. intercalate " "

borderTopRightRadius :: String -> Styles
borderTopRightRadius = declaration "border-top-right-radius"

borderTopRightRadiusJ :: Array String -> Styles
borderTopRightRadiusJ = declaration "border-top-right-radius" <. intercalate " "

borderTopStyle :: String -> Styles
borderTopStyle = declaration "border-top-style"

borderTopWidth :: String -> Styles
borderTopWidth = declaration "border-top-width"

borderWidth :: String -> Styles
borderWidth = declaration "border-width"

borderWidthJ :: Array String -> Styles
borderWidthJ = declaration "border-width" <. intercalate " "

bottom :: String -> Styles
bottom = declaration "bottom"

boxDecorationBreak :: String -> Styles
boxDecorationBreak = declaration "box-decoration-break"

boxShadow :: String -> Styles
boxShadow = declaration "box-shadow"

boxShadowJ :: Array String -> Styles
boxShadowJ = declaration "box-shadow" <. intercalate " "

boxShadowJJ :: Array (Array String) -> Styles
boxShadowJJ = declaration "box-shadow" <. intercalate ", " <. map (intercalate " ")

boxSizing :: String -> Styles
boxSizing = declaration "box-sizing"

boxSnap :: String -> Styles
boxSnap = declaration "box-snap"

breakAfter :: String -> Styles
breakAfter = declaration "break-after"

breakBefore :: String -> Styles
breakBefore = declaration "break-before"

breakInside :: String -> Styles
breakInside = declaration "break-inside"

captionSide :: String -> Styles
captionSide = declaration "caption-side"

caret :: String -> Styles
caret = declaration "caret"

caretJ :: Array String -> Styles
caretJ = declaration "caret" <. intercalate " "

caretColor :: String -> Styles
caretColor = declaration "caret-color"

caretShape :: String -> Styles
caretShape = declaration "caret-shape"

clear :: String -> Styles
clear = declaration "clear"

clip :: String -> Styles
clip = declaration "clip"

clipPath :: String -> Styles
clipPath = declaration "clip-path"

clipPathJ :: Array String -> Styles
clipPathJ = declaration "clip-path" <. intercalate " "

clipRule :: String -> Styles
clipRule = declaration "clip-rule"

color :: String -> Styles
color = declaration "color"

colorAdjust :: String -> Styles
colorAdjust = declaration "color-adjust"

colorInterpolationFilters :: String -> Styles
colorInterpolationFilters = declaration "color-interpolation-filters"

colorScheme :: String -> Styles
colorScheme = declaration "color-scheme"

colorSchemeJ :: Array String -> Styles
colorSchemeJ = declaration "color-scheme" <. intercalate " "

columnCount :: String -> Styles
columnCount = declaration "column-count"

columnFill :: String -> Styles
columnFill = declaration "column-fill"

columnGap :: String -> Styles
columnGap = declaration "column-gap"

columnRule :: String -> Styles
columnRule = declaration "column-rule"

columnRuleJ :: Array String -> Styles
columnRuleJ = declaration "column-rule" <. intercalate " "

columnRuleColor :: String -> Styles
columnRuleColor = declaration "column-rule-color"

columnRuleStyle :: String -> Styles
columnRuleStyle = declaration "column-rule-style"

columnRuleWidth :: String -> Styles
columnRuleWidth = declaration "column-rule-width"

columnSpan :: String -> Styles
columnSpan = declaration "column-span"

columnWidth :: String -> Styles
columnWidth = declaration "column-width"

columns :: String -> Styles
columns = declaration "columns"

columnsJ :: Array String -> Styles
columnsJ = declaration "columns" <. intercalate " "

contain :: String -> Styles
contain = declaration "contain"

containJ :: Array String -> Styles
containJ = declaration "contain" <. intercalate " "

content :: String -> Styles
content = declaration "content"

contentJ :: Array String -> Styles
contentJ = declaration "content" <. intercalate " "

continue :: String -> Styles
continue = declaration "continue"

counterIncrement :: String -> Styles
counterIncrement = declaration "counter-increment"

counterIncrementJ :: Array String -> Styles
counterIncrementJ = declaration "counter-increment" <. intercalate " "

counterIncrementJJ :: Array (Array String) -> Styles
counterIncrementJJ = declaration "counter-increment" <. intercalate " " <. map (intercalate " ")

counterReset :: String -> Styles
counterReset = declaration "counter-reset"

counterResetJ :: Array String -> Styles
counterResetJ = declaration "counter-reset" <. intercalate " "

counterResetJJ :: Array (Array String) -> Styles
counterResetJJ = declaration "counter-reset" <. intercalate " " <. map (intercalate " ")

counterSet :: String -> Styles
counterSet = declaration "counter-set"

counterSetJ :: Array String -> Styles
counterSetJ = declaration "counter-set" <. intercalate " "

counterSetJJ :: Array (Array String) -> Styles
counterSetJJ = declaration "counter-set" <. intercalate " " <. map (intercalate " ")

cue :: String -> Styles
cue = declaration "cue"

cueJ :: Array String -> Styles
cueJ = declaration "cue" <. intercalate " "

cueJJ :: Array (Array String) -> Styles
cueJJ = declaration "cue" <. intercalate " " <. map (intercalate " ")

cueAfter :: String -> Styles
cueAfter = declaration "cue-after"

cueAfterJ :: Array String -> Styles
cueAfterJ = declaration "cue-after" <. intercalate " "

cueBefore :: String -> Styles
cueBefore = declaration "cue-before"

cueBeforeJ :: Array String -> Styles
cueBeforeJ = declaration "cue-before" <. intercalate " "

cursor :: String -> Styles
cursor = declaration "cursor"

cursorJ :: Array String -> Styles
cursorJ = declaration "cursor" <. intercalate " "

cursorJJ :: Array (Array String) -> Styles
cursorJJ = declaration "cursor" <. intercalate " " <. map (intercalate " ")

direction :: String -> Styles
direction = declaration "direction"

display :: String -> Styles
display = declaration "display"

displayJ :: Array String -> Styles
displayJ = declaration "display" <. intercalate " "

dominantBaseline :: String -> Styles
dominantBaseline = declaration "dominant-baseline"

elevation :: String -> Styles
elevation = declaration "elevation"

emptyCells :: String -> Styles
emptyCells = declaration "empty-cells"

fill :: String -> Styles
fill = declaration "fill"

fillJ :: Array String -> Styles
fillJ = declaration "fill" <. intercalate " "

fillBreak :: String -> Styles
fillBreak = declaration "fill-break"

fillColor :: String -> Styles
fillColor = declaration "fill-color"

fillImage :: String -> Styles
fillImage = declaration "fill-image"

fillImageJ :: Array String -> Styles
fillImageJ = declaration "fill-image" <. intercalate ", "

fillOpacity :: String -> Styles
fillOpacity = declaration "fill-opacity"

fillOrigin :: String -> Styles
fillOrigin = declaration "fill-origin"

fillPosition :: String -> Styles
fillPosition = declaration "fill-position"

fillPositionJ :: Array String -> Styles
fillPositionJ = declaration "fill-position" <. intercalate " "

fillPositionJJ :: Array (Array String) -> Styles
fillPositionJJ = declaration "fill-position" <. intercalate ", " <. map (intercalate " ")

fillRepeat :: String -> Styles
fillRepeat = declaration "fill-repeat"

fillRepeatJ :: Array String -> Styles
fillRepeatJ = declaration "fill-repeat" <. intercalate " "

fillRepeatJJ :: Array (Array String) -> Styles
fillRepeatJJ = declaration "fill-repeat" <. intercalate ", " <. map (intercalate " ")

fillRule :: String -> Styles
fillRule = declaration "fill-rule"

fillSize :: String -> Styles
fillSize = declaration "fill-size"

fillSizeJ :: Array String -> Styles
fillSizeJ = declaration "fill-size" <. intercalate " "

fillSizeJJ :: Array (Array String) -> Styles
fillSizeJJ = declaration "fill-size" <. intercalate ", " <. map (intercalate " ")

filter :: String -> Styles
filter = declaration "filter"

filterJ :: Array String -> Styles
filterJ = declaration "filter" <. intercalate " "

flex :: String -> Styles
flex = declaration "flex"

flexJ :: Array String -> Styles
flexJ = declaration "flex" <. intercalate " "

flexBasis :: String -> Styles
flexBasis = declaration "flex-basis"

flexDirection :: String -> Styles
flexDirection = declaration "flex-direction"

flexFlow :: String -> Styles
flexFlow = declaration "flex-flow"

flexFlowJ :: Array String -> Styles
flexFlowJ = declaration "flex-flow" <. intercalate " "

flexGrow :: String -> Styles
flexGrow = declaration "flex-grow"

flexShrink :: String -> Styles
flexShrink = declaration "flex-shrink"

flexWrap :: String -> Styles
flexWrap = declaration "flex-wrap"

float :: String -> Styles
float = declaration "float"

floatDefer :: String -> Styles
floatDefer = declaration "float-defer"

floatOffset :: String -> Styles
floatOffset = declaration "float-offset"

floatReference :: String -> Styles
floatReference = declaration "float-reference"

floodColor :: String -> Styles
floodColor = declaration "flood-color"

floodOpacity :: String -> Styles
floodOpacity = declaration "flood-opacity"

flowFrom :: String -> Styles
flowFrom = declaration "flow-from"

flowInto :: String -> Styles
flowInto = declaration "flow-into"

flowIntoJ :: Array String -> Styles
flowIntoJ = declaration "flow-into" <. intercalate " "

font :: String -> Styles
font = declaration "font"

fontJ :: Array String -> Styles
fontJ = declaration "font" <. intercalate " "

fontFamily :: String -> Styles
fontFamily = declaration "font-family"

fontFamilyJ :: Array String -> Styles
fontFamilyJ = declaration "font-family" <. intercalate ", "

fontFeatureSettings :: String -> Styles
fontFeatureSettings = declaration "font-feature-settings"

fontFeatureSettingsJ :: Array String -> Styles
fontFeatureSettingsJ = declaration "font-feature-settings" <. intercalate " "

fontFeatureSettingsJJ :: Array (Array String) -> Styles
fontFeatureSettingsJJ = declaration "font-feature-settings" <. intercalate ", " <. map (intercalate " ")

fontKerning :: String -> Styles
fontKerning = declaration "font-kerning"

fontLanguageOverride :: String -> Styles
fontLanguageOverride = declaration "font-language-override"

fontOpticalSizing :: String -> Styles
fontOpticalSizing = declaration "font-optical-sizing"

fontPalette :: String -> Styles
fontPalette = declaration "font-palette"

fontSize :: String -> Styles
fontSize = declaration "font-size"

fontSizeAdjust :: String -> Styles
fontSizeAdjust = declaration "font-size-adjust"

fontStretch :: String -> Styles
fontStretch = declaration "font-stretch"

fontStyle :: String -> Styles
fontStyle = declaration "font-style"

fontStyleJ :: Array String -> Styles
fontStyleJ = declaration "font-style" <. intercalate " "

fontSynthesis :: String -> Styles
fontSynthesis = declaration "font-synthesis"

fontSynthesisJ :: Array String -> Styles
fontSynthesisJ = declaration "font-synthesis" <. intercalate " "

fontSynthesisSmallCaps :: String -> Styles
fontSynthesisSmallCaps = declaration "font-synthesis-small-caps"

fontSynthesisStyle :: String -> Styles
fontSynthesisStyle = declaration "font-synthesis-style"

fontSynthesisWeight :: String -> Styles
fontSynthesisWeight = declaration "font-synthesis-weight"

fontVariant :: String -> Styles
fontVariant = declaration "font-variant"

fontVariantJ :: Array String -> Styles
fontVariantJ = declaration "font-variant" <. intercalate " "

fontVariantAlternates :: String -> Styles
fontVariantAlternates = declaration "font-variant-alternates"

fontVariantAlternatesJ :: Array String -> Styles
fontVariantAlternatesJ = declaration "font-variant-alternates" <. intercalate " "

fontVariantCaps :: String -> Styles
fontVariantCaps = declaration "font-variant-caps"

fontVariantEastAsian :: String -> Styles
fontVariantEastAsian = declaration "font-variant-east-asian"

fontVariantEastAsianJ :: Array String -> Styles
fontVariantEastAsianJ = declaration "font-variant-east-asian" <. intercalate " "

fontVariantEmoji :: String -> Styles
fontVariantEmoji = declaration "font-variant-emoji"

fontVariantLigatures :: String -> Styles
fontVariantLigatures = declaration "font-variant-ligatures"

fontVariantLigaturesJ :: Array String -> Styles
fontVariantLigaturesJ = declaration "font-variant-ligatures" <. intercalate " "

fontVariantNumeric :: String -> Styles
fontVariantNumeric = declaration "font-variant-numeric"

fontVariantNumericJ :: Array String -> Styles
fontVariantNumericJ = declaration "font-variant-numeric" <. intercalate " "

fontVariantPosition :: String -> Styles
fontVariantPosition = declaration "font-variant-position"

fontVariationSettings :: String -> Styles
fontVariationSettings = declaration "font-variation-settings"

fontVariationSettingsJ :: Array String -> Styles
fontVariationSettingsJ = declaration "font-variation-settings" <. intercalate " "

fontVariationSettingsJJ :: Array (Array String) -> Styles
fontVariationSettingsJJ = declaration "font-variation-settings" <. intercalate ", " <. map (intercalate " ")

fontWeight :: String -> Styles
fontWeight = declaration "font-weight"

footnoteDisplay :: String -> Styles
footnoteDisplay = declaration "footnote-display"

footnotePolicy :: String -> Styles
footnotePolicy = declaration "footnote-policy"

forcedColorAdjust :: String -> Styles
forcedColorAdjust = declaration "forced-color-adjust"

gap :: String -> Styles
gap = declaration "gap"

gapJ :: Array String -> Styles
gapJ = declaration "gap" <. intercalate " "

glyphOrientationVertical :: String -> Styles
glyphOrientationVertical = declaration "glyph-orientation-vertical"

grid :: String -> Styles
grid = declaration "grid"

gridJ :: Array String -> Styles
gridJ = declaration "grid" <. intercalate " "

gridJJ :: Array (Array String) -> Styles
gridJJ = declaration "grid" <. intercalate " " <. map (intercalate " ")

gridArea :: String -> Styles
gridArea = declaration "grid-area"

gridAreaJ :: Array String -> Styles
gridAreaJ = declaration "grid-area" <. intercalate " "

gridAreaJJ :: Array (Array String) -> Styles
gridAreaJJ = declaration "grid-area" <. intercalate " " <. map (intercalate " ")

gridAutoColumns :: String -> Styles
gridAutoColumns = declaration "grid-auto-columns"

gridAutoColumnsJ :: Array String -> Styles
gridAutoColumnsJ = declaration "grid-auto-columns" <. intercalate " "

gridAutoFlow :: String -> Styles
gridAutoFlow = declaration "grid-auto-flow"

gridAutoFlowJ :: Array String -> Styles
gridAutoFlowJ = declaration "grid-auto-flow" <. intercalate " "

gridAutoRows :: String -> Styles
gridAutoRows = declaration "grid-auto-rows"

gridAutoRowsJ :: Array String -> Styles
gridAutoRowsJ = declaration "grid-auto-rows" <. intercalate " "

gridColumn :: String -> Styles
gridColumn = declaration "grid-column"

gridColumnJ :: Array String -> Styles
gridColumnJ = declaration "grid-column" <. intercalate " "

gridColumnJJ :: Array (Array String) -> Styles
gridColumnJJ = declaration "grid-column" <. intercalate " " <. map (intercalate " ")

gridColumnEnd :: String -> Styles
gridColumnEnd = declaration "grid-column-end"

gridColumnEndJ :: Array String -> Styles
gridColumnEndJ = declaration "grid-column-end" <. intercalate " "

gridColumnStart :: String -> Styles
gridColumnStart = declaration "grid-column-start"

gridColumnStartJ :: Array String -> Styles
gridColumnStartJ = declaration "grid-column-start" <. intercalate " "

gridRow :: String -> Styles
gridRow = declaration "grid-row"

gridRowJ :: Array String -> Styles
gridRowJ = declaration "grid-row" <. intercalate " "

gridRowJJ :: Array (Array String) -> Styles
gridRowJJ = declaration "grid-row" <. intercalate " " <. map (intercalate " ")

gridRowEnd :: String -> Styles
gridRowEnd = declaration "grid-row-end"

gridRowEndJ :: Array String -> Styles
gridRowEndJ = declaration "grid-row-end" <. intercalate " "

gridRowStart :: String -> Styles
gridRowStart = declaration "grid-row-start"

gridRowStartJ :: Array String -> Styles
gridRowStartJ = declaration "grid-row-start" <. intercalate " "

gridTemplate :: String -> Styles
gridTemplate = declaration "grid-template"

gridTemplateJ :: Array String -> Styles
gridTemplateJ = declaration "grid-template" <. intercalate " "

gridTemplateJJ :: Array (Array String) -> Styles
gridTemplateJJ = declaration "grid-template" <. intercalate " " <. map (intercalate " ")

gridTemplateAreas :: String -> Styles
gridTemplateAreas = declaration "grid-template-areas"

gridTemplateAreasJ :: Array String -> Styles
gridTemplateAreasJ = declaration "grid-template-areas" <. intercalate " "

gridTemplateColumns :: String -> Styles
gridTemplateColumns = declaration "grid-template-columns"

gridTemplateColumnsJ :: Array String -> Styles
gridTemplateColumnsJ = declaration "grid-template-columns" <. intercalate " "

gridTemplateColumnsJJ :: Array (Array String) -> Styles
gridTemplateColumnsJJ = declaration "grid-template-columns" <. intercalate " " <. map (intercalate " ")

gridTemplateRows :: String -> Styles
gridTemplateRows = declaration "grid-template-rows"

gridTemplateRowsJ :: Array String -> Styles
gridTemplateRowsJ = declaration "grid-template-rows" <. intercalate " "

gridTemplateRowsJJ :: Array (Array String) -> Styles
gridTemplateRowsJJ = declaration "grid-template-rows" <. intercalate " " <. map (intercalate " ")

hangingPunctuation :: String -> Styles
hangingPunctuation = declaration "hanging-punctuation"

hangingPunctuationJ :: Array String -> Styles
hangingPunctuationJ = declaration "hanging-punctuation" <. intercalate " "

height :: String -> Styles
height = declaration "height"

hyphenateCharacter :: String -> Styles
hyphenateCharacter = declaration "hyphenate-character"

hyphenateLimitChars :: String -> Styles
hyphenateLimitChars = declaration "hyphenate-limit-chars"

hyphenateLimitCharsJ :: Array String -> Styles
hyphenateLimitCharsJ = declaration "hyphenate-limit-chars" <. intercalate " "

hyphenateLimitLast :: String -> Styles
hyphenateLimitLast = declaration "hyphenate-limit-last"

hyphenateLimitLines :: String -> Styles
hyphenateLimitLines = declaration "hyphenate-limit-lines"

hyphenateLimitZone :: String -> Styles
hyphenateLimitZone = declaration "hyphenate-limit-zone"

hyphens :: String -> Styles
hyphens = declaration "hyphens"

imageOrientation :: String -> Styles
imageOrientation = declaration "image-orientation"

imageOrientationJ :: Array String -> Styles
imageOrientationJ = declaration "image-orientation" <. intercalate " "

imageRendering :: String -> Styles
imageRendering = declaration "image-rendering"

imageResolution :: String -> Styles
imageResolution = declaration "image-resolution"

imageResolutionJ :: Array String -> Styles
imageResolutionJ = declaration "image-resolution" <. intercalate " "

initialLetters :: String -> Styles
initialLetters = declaration "initial-letters"

initialLettersJ :: Array String -> Styles
initialLettersJ = declaration "initial-letters" <. intercalate " "

initialLettersAlign :: String -> Styles
initialLettersAlign = declaration "initial-letters-align"

initialLettersAlignJ :: Array String -> Styles
initialLettersAlignJ = declaration "initial-letters-align" <. intercalate " "

initialLettersWrap :: String -> Styles
initialLettersWrap = declaration "initial-letters-wrap"

inlineSize :: String -> Styles
inlineSize = declaration "inline-size"

inlineSizing :: String -> Styles
inlineSizing = declaration "inline-sizing"

inset :: String -> Styles
inset = declaration "inset"

insetJ :: Array String -> Styles
insetJ = declaration "inset" <. intercalate " "

insetBlock :: String -> Styles
insetBlock = declaration "inset-block"

insetBlockJ :: Array String -> Styles
insetBlockJ = declaration "inset-block" <. intercalate " "

insetBlockEnd :: String -> Styles
insetBlockEnd = declaration "inset-block-end"

insetBlockStart :: String -> Styles
insetBlockStart = declaration "inset-block-start"

insetInline :: String -> Styles
insetInline = declaration "inset-inline"

insetInlineJ :: Array String -> Styles
insetInlineJ = declaration "inset-inline" <. intercalate " "

insetInlineEnd :: String -> Styles
insetInlineEnd = declaration "inset-inline-end"

insetInlineStart :: String -> Styles
insetInlineStart = declaration "inset-inline-start"

isolation :: String -> Styles
isolation = declaration "isolation"

justifyContent :: String -> Styles
justifyContent = declaration "justify-content"

justifyContentJ :: Array String -> Styles
justifyContentJ = declaration "justify-content" <. intercalate " "

justifyItems :: String -> Styles
justifyItems = declaration "justify-items"

justifyItemsJ :: Array String -> Styles
justifyItemsJ = declaration "justify-items" <. intercalate " "

justifySelf :: String -> Styles
justifySelf = declaration "justify-self"

justifySelfJ :: Array String -> Styles
justifySelfJ = declaration "justify-self" <. intercalate " "

left :: String -> Styles
left = declaration "left"

letterSpacing :: String -> Styles
letterSpacing = declaration "letter-spacing"

lightingColor :: String -> Styles
lightingColor = declaration "lighting-color"

lineBreak :: String -> Styles
lineBreak = declaration "line-break"

lineClamp :: String -> Styles
lineClamp = declaration "line-clamp"

lineClampJ :: Array String -> Styles
lineClampJ = declaration "line-clamp" <. intercalate " "

lineGrid :: String -> Styles
lineGrid = declaration "line-grid"

lineHeight :: String -> Styles
lineHeight = declaration "line-height"

lineHeightStep :: String -> Styles
lineHeightStep = declaration "line-height-step"

linePadding :: String -> Styles
linePadding = declaration "line-padding"

lineSnap :: String -> Styles
lineSnap = declaration "line-snap"

listStyle :: String -> Styles
listStyle = declaration "list-style"

listStyleJ :: Array String -> Styles
listStyleJ = declaration "list-style" <. intercalate " "

listStyleImage :: String -> Styles
listStyleImage = declaration "list-style-image"

listStylePosition :: String -> Styles
listStylePosition = declaration "list-style-position"

listStyleType :: String -> Styles
listStyleType = declaration "list-style-type"

margin :: String -> Styles
margin = declaration "margin"

marginJ :: Array String -> Styles
marginJ = declaration "margin" <. intercalate " "

marginBlock :: String -> Styles
marginBlock = declaration "margin-block"

marginBlockJ :: Array String -> Styles
marginBlockJ = declaration "margin-block" <. intercalate " "

marginBlockEnd :: String -> Styles
marginBlockEnd = declaration "margin-block-end"

marginBlockStart :: String -> Styles
marginBlockStart = declaration "margin-block-start"

marginBottom :: String -> Styles
marginBottom = declaration "margin-bottom"

marginBreak :: String -> Styles
marginBreak = declaration "margin-break"

marginInline :: String -> Styles
marginInline = declaration "margin-inline"

marginInlineJ :: Array String -> Styles
marginInlineJ = declaration "margin-inline" <. intercalate " "

marginInlineEnd :: String -> Styles
marginInlineEnd = declaration "margin-inline-end"

marginInlineStart :: String -> Styles
marginInlineStart = declaration "margin-inline-start"

marginLeft :: String -> Styles
marginLeft = declaration "margin-left"

marginRight :: String -> Styles
marginRight = declaration "margin-right"

marginTop :: String -> Styles
marginTop = declaration "margin-top"

marginTrim :: String -> Styles
marginTrim = declaration "margin-trim"

marker :: String -> Styles
marker = declaration "marker"

markerJ :: Array String -> Styles
markerJ = declaration "marker" <. intercalate " "

markerEnd :: String -> Styles
markerEnd = declaration "marker-end"

markerKnockoutLeft :: String -> Styles
markerKnockoutLeft = declaration "marker-knockout-left"

markerKnockoutLeftJ :: Array String -> Styles
markerKnockoutLeftJ = declaration "marker-knockout-left" <. intercalate " "

markerKnockoutRight :: String -> Styles
markerKnockoutRight = declaration "marker-knockout-right"

markerKnockoutRightJ :: Array String -> Styles
markerKnockoutRightJ = declaration "marker-knockout-right" <. intercalate " "

markerMid :: String -> Styles
markerMid = declaration "marker-mid"

markerPattern :: String -> Styles
markerPattern = declaration "marker-pattern"

markerPatternJ :: Array String -> Styles
markerPatternJ = declaration "marker-pattern" <. intercalate " "

markerPatternJJ :: Array (Array String) -> Styles
markerPatternJJ = declaration "marker-pattern" <. intercalate " " <. map (intercalate " ")

markerSegment :: String -> Styles
markerSegment = declaration "marker-segment"

markerSide :: String -> Styles
markerSide = declaration "marker-side"

markerStart :: String -> Styles
markerStart = declaration "marker-start"

mask :: String -> Styles
mask = declaration "mask"

maskJ :: Array String -> Styles
maskJ = declaration "mask" <. intercalate " "

maskJJ :: Array (Array String) -> Styles
maskJJ = declaration "mask" <. intercalate ", " <. map (intercalate " ")

maskBorder :: String -> Styles
maskBorder = declaration "mask-border"

maskBorderJ :: Array String -> Styles
maskBorderJ = declaration "mask-border" <. intercalate " "

maskBorderJJ :: Array (Array String) -> Styles
maskBorderJJ = declaration "mask-border" <. intercalate " " <. map (intercalate " ")

maskBorderMode :: String -> Styles
maskBorderMode = declaration "mask-border-mode"

maskBorderOutset :: String -> Styles
maskBorderOutset = declaration "mask-border-outset"

maskBorderOutsetJ :: Array String -> Styles
maskBorderOutsetJ = declaration "mask-border-outset" <. intercalate " "

maskBorderRepeat :: String -> Styles
maskBorderRepeat = declaration "mask-border-repeat"

maskBorderRepeatJ :: Array String -> Styles
maskBorderRepeatJ = declaration "mask-border-repeat" <. intercalate " "

maskBorderSlice :: String -> Styles
maskBorderSlice = declaration "mask-border-slice"

maskBorderSliceJ :: Array String -> Styles
maskBorderSliceJ = declaration "mask-border-slice" <. intercalate " "

maskBorderSliceJJ :: Array (Array String) -> Styles
maskBorderSliceJJ = declaration "mask-border-slice" <. intercalate " " <. map (intercalate " ")

maskBorderSource :: String -> Styles
maskBorderSource = declaration "mask-border-source"

maskBorderWidth :: String -> Styles
maskBorderWidth = declaration "mask-border-width"

maskBorderWidthJ :: Array String -> Styles
maskBorderWidthJ = declaration "mask-border-width" <. intercalate " "

maskClip :: String -> Styles
maskClip = declaration "mask-clip"

maskClipJ :: Array String -> Styles
maskClipJ = declaration "mask-clip" <. intercalate ", "

maskComposite :: String -> Styles
maskComposite = declaration "mask-composite"

maskCompositeJ :: Array String -> Styles
maskCompositeJ = declaration "mask-composite" <. intercalate ", "

maskImage :: String -> Styles
maskImage = declaration "mask-image"

maskImageJ :: Array String -> Styles
maskImageJ = declaration "mask-image" <. intercalate ", "

maskMode :: String -> Styles
maskMode = declaration "mask-mode"

maskModeJ :: Array String -> Styles
maskModeJ = declaration "mask-mode" <. intercalate ", "

maskOrigin :: String -> Styles
maskOrigin = declaration "mask-origin"

maskOriginJ :: Array String -> Styles
maskOriginJ = declaration "mask-origin" <. intercalate ", "

maskPosition :: String -> Styles
maskPosition = declaration "mask-position"

maskPositionJ :: Array String -> Styles
maskPositionJ = declaration "mask-position" <. intercalate ", "

maskRepeat :: String -> Styles
maskRepeat = declaration "mask-repeat"

maskRepeatJ :: Array String -> Styles
maskRepeatJ = declaration "mask-repeat" <. intercalate ", "

maskSize :: String -> Styles
maskSize = declaration "mask-size"

maskSizeJ :: Array String -> Styles
maskSizeJ = declaration "mask-size" <. intercalate ", "

maskType :: String -> Styles
maskType = declaration "mask-type"

maxBlockSize :: String -> Styles
maxBlockSize = declaration "max-block-size"

maxHeight :: String -> Styles
maxHeight = declaration "max-height"

maxInlineSize :: String -> Styles
maxInlineSize = declaration "max-inline-size"

maxLines :: String -> Styles
maxLines = declaration "max-lines"

maxWidth :: String -> Styles
maxWidth = declaration "max-width"

minBlockSize :: String -> Styles
minBlockSize = declaration "min-block-size"

minHeight :: String -> Styles
minHeight = declaration "min-height"

minInlineSize :: String -> Styles
minInlineSize = declaration "min-inline-size"

minWidth :: String -> Styles
minWidth = declaration "min-width"

mixBlendMode :: String -> Styles
mixBlendMode = declaration "mix-blend-mode"

navDown :: String -> Styles
navDown = declaration "nav-down"

navDownJ :: Array String -> Styles
navDownJ = declaration "nav-down" <. intercalate " "

navLeft :: String -> Styles
navLeft = declaration "nav-left"

navLeftJ :: Array String -> Styles
navLeftJ = declaration "nav-left" <. intercalate " "

navRight :: String -> Styles
navRight = declaration "nav-right"

navRightJ :: Array String -> Styles
navRightJ = declaration "nav-right" <. intercalate " "

navUp :: String -> Styles
navUp = declaration "nav-up"

navUpJ :: Array String -> Styles
navUpJ = declaration "nav-up" <. intercalate " "

objectFit :: String -> Styles
objectFit = declaration "object-fit"

objectPosition :: String -> Styles
objectPosition = declaration "object-position"

objectPositionJ :: Array String -> Styles
objectPositionJ = declaration "object-position" <. intercalate " "

offset :: String -> Styles
offset = declaration "offset"

offsetJ :: Array String -> Styles
offsetJ = declaration "offset" <. intercalate " "

offsetJJ :: Array (Array String) -> Styles
offsetJJ = declaration "offset" <. intercalate " " <. map (intercalate " ")

offsetAfter :: String -> Styles
offsetAfter = declaration "offset-after"

offsetAnchor :: String -> Styles
offsetAnchor = declaration "offset-anchor"

offsetAnchorJ :: Array String -> Styles
offsetAnchorJ = declaration "offset-anchor" <. intercalate " "

offsetBefore :: String -> Styles
offsetBefore = declaration "offset-before"

offsetDistance :: String -> Styles
offsetDistance = declaration "offset-distance"

offsetEnd :: String -> Styles
offsetEnd = declaration "offset-end"

offsetPath :: String -> Styles
offsetPath = declaration "offset-path"

offsetPathJ :: Array String -> Styles
offsetPathJ = declaration "offset-path" <. intercalate " "

offsetPosition :: String -> Styles
offsetPosition = declaration "offset-position"

offsetPositionJ :: Array String -> Styles
offsetPositionJ = declaration "offset-position" <. intercalate " "

offsetRotate :: String -> Styles
offsetRotate = declaration "offset-rotate"

offsetStart :: String -> Styles
offsetStart = declaration "offset-start"

opacity :: String -> Styles
opacity = declaration "opacity"

order :: String -> Styles
order = declaration "order"

orphans :: String -> Styles
orphans = declaration "orphans"

outline :: String -> Styles
outline = declaration "outline"

outlineJ :: Array String -> Styles
outlineJ = declaration "outline" <. intercalate " "

outlineColor :: String -> Styles
outlineColor = declaration "outline-color"

outlineOffset :: String -> Styles
outlineOffset = declaration "outline-offset"

outlineStyle :: String -> Styles
outlineStyle = declaration "outline-style"

outlineWidth :: String -> Styles
outlineWidth = declaration "outline-width"

overflow :: String -> Styles
overflow = declaration "overflow"

overflowJ :: Array String -> Styles
overflowJ = declaration "overflow" <. intercalate " "

overflowAnchor :: String -> Styles
overflowAnchor = declaration "overflow-anchor"

overflowBlock :: String -> Styles
overflowBlock = declaration "overflow-block"

overflowBlockJ :: Array String -> Styles
overflowBlockJ = declaration "overflow-block" <. intercalate " "

overflowInline :: String -> Styles
overflowInline = declaration "overflow-inline"

overflowInlineJ :: Array String -> Styles
overflowInlineJ = declaration "overflow-inline" <. intercalate " "

overflowWrap :: String -> Styles
overflowWrap = declaration "overflow-wrap"

overflowX :: String -> Styles
overflowX = declaration "overflow-x"

overflowY :: String -> Styles
overflowY = declaration "overflow-y"

overscrollBehavior :: String -> Styles
overscrollBehavior = declaration "overscroll-behavior"

overscrollBehaviorJ :: Array String -> Styles
overscrollBehaviorJ = declaration "overscroll-behavior" <. intercalate " "

overscrollBehaviorBlock :: String -> Styles
overscrollBehaviorBlock = declaration "overscroll-behavior-block"

overscrollBehaviorInline :: String -> Styles
overscrollBehaviorInline = declaration "overscroll-behavior-inline"

overscrollBehaviorX :: String -> Styles
overscrollBehaviorX = declaration "overscroll-behavior-x"

overscrollBehaviorY :: String -> Styles
overscrollBehaviorY = declaration "overscroll-behavior-y"

padding :: String -> Styles
padding = declaration "padding"

paddingJ :: Array String -> Styles
paddingJ = declaration "padding" <. intercalate " "

paddingBlock :: String -> Styles
paddingBlock = declaration "padding-block"

paddingBlockJ :: Array String -> Styles
paddingBlockJ = declaration "padding-block" <. intercalate " "

paddingBlockEnd :: String -> Styles
paddingBlockEnd = declaration "padding-block-end"

paddingBlockStart :: String -> Styles
paddingBlockStart = declaration "padding-block-start"

paddingBottom :: String -> Styles
paddingBottom = declaration "padding-bottom"

paddingInline :: String -> Styles
paddingInline = declaration "padding-inline"

paddingInlineJ :: Array String -> Styles
paddingInlineJ = declaration "padding-inline" <. intercalate " "

paddingInlineEnd :: String -> Styles
paddingInlineEnd = declaration "padding-inline-end"

paddingInlineStart :: String -> Styles
paddingInlineStart = declaration "padding-inline-start"

paddingLeft :: String -> Styles
paddingLeft = declaration "padding-left"

paddingRight :: String -> Styles
paddingRight = declaration "padding-right"

paddingTop :: String -> Styles
paddingTop = declaration "padding-top"

page :: String -> Styles
page = declaration "page"

pageBreakAfter :: String -> Styles
pageBreakAfter = declaration "page-break-after"

pageBreakBefore :: String -> Styles
pageBreakBefore = declaration "page-break-before"

pageBreakInside :: String -> Styles
pageBreakInside = declaration "page-break-inside"

pause :: String -> Styles
pause = declaration "pause"

pauseJ :: Array String -> Styles
pauseJ = declaration "pause" <. intercalate " "

pauseAfter :: String -> Styles
pauseAfter = declaration "pause-after"

pauseBefore :: String -> Styles
pauseBefore = declaration "pause-before"

perspective :: String -> Styles
perspective = declaration "perspective"

perspectiveOrigin :: String -> Styles
perspectiveOrigin = declaration "perspective-origin"

perspectiveOriginJ :: Array String -> Styles
perspectiveOriginJ = declaration "perspective-origin" <. intercalate " "

pitch :: String -> Styles
pitch = declaration "pitch"

pitchRange :: String -> Styles
pitchRange = declaration "pitch-range"

placeContent :: String -> Styles
placeContent = declaration "place-content"

placeContentJ :: Array String -> Styles
placeContentJ = declaration "place-content" <. intercalate " "

placeItems :: String -> Styles
placeItems = declaration "place-items"

placeItemsJ :: Array String -> Styles
placeItemsJ = declaration "place-items" <. intercalate " "

placeSelf :: String -> Styles
placeSelf = declaration "place-self"

placeSelfJ :: Array String -> Styles
placeSelfJ = declaration "place-self" <. intercalate " "

playDuring :: String -> Styles
playDuring = declaration "play-during"

playDuringJ :: Array String -> Styles
playDuringJ = declaration "play-during" <. intercalate " "

pointerEvents :: String -> Styles
pointerEvents = declaration "pointer-events"

position :: String -> Styles
position = declaration "position"

quotes :: String -> Styles
quotes = declaration "quotes"

quotesJ :: Array String -> Styles
quotesJ = declaration "quotes" <. intercalate " "

quotesJJ :: Array (Array String) -> Styles
quotesJJ = declaration "quotes" <. intercalate " " <. map (intercalate " ")

regionFragment :: String -> Styles
regionFragment = declaration "region-fragment"

resize :: String -> Styles
resize = declaration "resize"

rest :: String -> Styles
rest = declaration "rest"

restJ :: Array String -> Styles
restJ = declaration "rest" <. intercalate " "

restAfter :: String -> Styles
restAfter = declaration "rest-after"

restBefore :: String -> Styles
restBefore = declaration "rest-before"

richness :: String -> Styles
richness = declaration "richness"

right :: String -> Styles
right = declaration "right"

rotate :: String -> Styles
rotate = declaration "rotate"

rotateJ :: Array String -> Styles
rotateJ = declaration "rotate" <. intercalate " "

rowGap :: String -> Styles
rowGap = declaration "row-gap"

rubyAlign :: String -> Styles
rubyAlign = declaration "ruby-align"

rubyAlignJ :: Array String -> Styles
rubyAlignJ = declaration "ruby-align" <. intercalate " "

rubyMerge :: String -> Styles
rubyMerge = declaration "ruby-merge"

rubyPosition :: String -> Styles
rubyPosition = declaration "ruby-position"

rubyPositionJ :: Array String -> Styles
rubyPositionJ = declaration "ruby-position" <. intercalate " "

running :: String -> Styles
running = declaration "running"

scale :: String -> Styles
scale = declaration "scale"

scaleJ :: Array String -> Styles
scaleJ = declaration "scale" <. intercalate " "

scrollBehavior :: String -> Styles
scrollBehavior = declaration "scroll-behavior"

scrollMargin :: String -> Styles
scrollMargin = declaration "scroll-margin"

scrollMarginJ :: Array String -> Styles
scrollMarginJ = declaration "scroll-margin" <. intercalate " "

scrollMarginBlock :: String -> Styles
scrollMarginBlock = declaration "scroll-margin-block"

scrollMarginBlockJ :: Array String -> Styles
scrollMarginBlockJ = declaration "scroll-margin-block" <. intercalate " "

scrollMarginBlockEnd :: String -> Styles
scrollMarginBlockEnd = declaration "scroll-margin-block-end"

scrollMarginBlockStart :: String -> Styles
scrollMarginBlockStart = declaration "scroll-margin-block-start"

scrollMarginBottom :: String -> Styles
scrollMarginBottom = declaration "scroll-margin-bottom"

scrollMarginInline :: String -> Styles
scrollMarginInline = declaration "scroll-margin-inline"

scrollMarginInlineJ :: Array String -> Styles
scrollMarginInlineJ = declaration "scroll-margin-inline" <. intercalate " "

scrollMarginInlineEnd :: String -> Styles
scrollMarginInlineEnd = declaration "scroll-margin-inline-end"

scrollMarginInlineStart :: String -> Styles
scrollMarginInlineStart = declaration "scroll-margin-inline-start"

scrollMarginLeft :: String -> Styles
scrollMarginLeft = declaration "scroll-margin-left"

scrollMarginRight :: String -> Styles
scrollMarginRight = declaration "scroll-margin-right"

scrollMarginTop :: String -> Styles
scrollMarginTop = declaration "scroll-margin-top"

scrollPadding :: String -> Styles
scrollPadding = declaration "scroll-padding"

scrollPaddingJ :: Array String -> Styles
scrollPaddingJ = declaration "scroll-padding" <. intercalate " "

scrollPaddingBlock :: String -> Styles
scrollPaddingBlock = declaration "scroll-padding-block"

scrollPaddingBlockJ :: Array String -> Styles
scrollPaddingBlockJ = declaration "scroll-padding-block" <. intercalate " "

scrollPaddingBlockEnd :: String -> Styles
scrollPaddingBlockEnd = declaration "scroll-padding-block-end"

scrollPaddingBlockStart :: String -> Styles
scrollPaddingBlockStart = declaration "scroll-padding-block-start"

scrollPaddingBottom :: String -> Styles
scrollPaddingBottom = declaration "scroll-padding-bottom"

scrollPaddingInline :: String -> Styles
scrollPaddingInline = declaration "scroll-padding-inline"

scrollPaddingInlineJ :: Array String -> Styles
scrollPaddingInlineJ = declaration "scroll-padding-inline" <. intercalate " "

scrollPaddingInlineEnd :: String -> Styles
scrollPaddingInlineEnd = declaration "scroll-padding-inline-end"

scrollPaddingInlineStart :: String -> Styles
scrollPaddingInlineStart = declaration "scroll-padding-inline-start"

scrollPaddingLeft :: String -> Styles
scrollPaddingLeft = declaration "scroll-padding-left"

scrollPaddingRight :: String -> Styles
scrollPaddingRight = declaration "scroll-padding-right"

scrollPaddingTop :: String -> Styles
scrollPaddingTop = declaration "scroll-padding-top"

scrollSnapAlign :: String -> Styles
scrollSnapAlign = declaration "scroll-snap-align"

scrollSnapAlignJ :: Array String -> Styles
scrollSnapAlignJ = declaration "scroll-snap-align" <. intercalate " "

scrollSnapStop :: String -> Styles
scrollSnapStop = declaration "scroll-snap-stop"

scrollSnapType :: String -> Styles
scrollSnapType = declaration "scroll-snap-type"

scrollSnapTypeJ :: Array String -> Styles
scrollSnapTypeJ = declaration "scroll-snap-type" <. intercalate " "

scrollbarColor :: String -> Styles
scrollbarColor = declaration "scrollbar-color"

scrollbarColorJ :: Array String -> Styles
scrollbarColorJ = declaration "scrollbar-color" <. intercalate " "

scrollbarGutter :: String -> Styles
scrollbarGutter = declaration "scrollbar-gutter"

scrollbarGutterJ :: Array String -> Styles
scrollbarGutterJ = declaration "scrollbar-gutter" <. intercalate " "

scrollbarWidth :: String -> Styles
scrollbarWidth = declaration "scrollbar-width"

shapeImageThreshold :: String -> Styles
shapeImageThreshold = declaration "shape-image-threshold"

shapeInside :: String -> Styles
shapeInside = declaration "shape-inside"

shapeInsideJ :: Array String -> Styles
shapeInsideJ = declaration "shape-inside" <. intercalate " "

shapeMargin :: String -> Styles
shapeMargin = declaration "shape-margin"

shapeOutside :: String -> Styles
shapeOutside = declaration "shape-outside"

shapeOutsideJ :: Array String -> Styles
shapeOutsideJ = declaration "shape-outside" <. intercalate " "

spatialNavigationAction :: String -> Styles
spatialNavigationAction = declaration "spatial-navigation-action"

spatialNavigationContain :: String -> Styles
spatialNavigationContain = declaration "spatial-navigation-contain"

spatialNavigationFunction :: String -> Styles
spatialNavigationFunction = declaration "spatial-navigation-function"

speak :: String -> Styles
speak = declaration "speak"

speakAs :: String -> Styles
speakAs = declaration "speak-as"

speakAsJ :: Array String -> Styles
speakAsJ = declaration "speak-as" <. intercalate " "

speakHeader :: String -> Styles
speakHeader = declaration "speak-header"

speakNumeral :: String -> Styles
speakNumeral = declaration "speak-numeral"

speakPunctuation :: String -> Styles
speakPunctuation = declaration "speak-punctuation"

speechRate :: String -> Styles
speechRate = declaration "speech-rate"

stress :: String -> Styles
stress = declaration "stress"

stringSet :: String -> Styles
stringSet = declaration "string-set"

stringSetJ :: Array String -> Styles
stringSetJ = declaration "string-set" <. intercalate " "

stringSetJJ :: Array (Array String) -> Styles
stringSetJJ = declaration "string-set" <. intercalate ", " <. map (intercalate " ")

stroke :: String -> Styles
stroke = declaration "stroke"

strokeJ :: Array String -> Styles
strokeJ = declaration "stroke" <. intercalate " "

strokeJJ :: Array (Array String) -> Styles
strokeJJ = declaration "stroke" <. intercalate ", " <. map (intercalate " ")

strokeAlign :: String -> Styles
strokeAlign = declaration "stroke-align"

strokeAlignment :: String -> Styles
strokeAlignment = declaration "stroke-alignment"

strokeBreak :: String -> Styles
strokeBreak = declaration "stroke-break"

strokeColor :: String -> Styles
strokeColor = declaration "stroke-color"

strokeColorJ :: Array String -> Styles
strokeColorJ = declaration "stroke-color" <. intercalate ", "

strokeDashCorner :: String -> Styles
strokeDashCorner = declaration "stroke-dash-corner"

strokeDashJustify :: String -> Styles
strokeDashJustify = declaration "stroke-dash-justify"

strokeDashJustifyJ :: Array String -> Styles
strokeDashJustifyJ = declaration "stroke-dash-justify" <. intercalate " "

strokeDashadjust :: String -> Styles
strokeDashadjust = declaration "stroke-dashadjust"

strokeDashadjustJ :: Array String -> Styles
strokeDashadjustJ = declaration "stroke-dashadjust" <. intercalate " "

strokeDasharray :: String -> Styles
strokeDasharray = declaration "stroke-dasharray"

strokeDasharrayJ :: Array String -> Styles
strokeDasharrayJ = declaration "stroke-dasharray" <. intercalate " "

strokeDasharrayJJ :: Array (Array String) -> Styles
strokeDasharrayJJ = declaration "stroke-dasharray" <. intercalate ", " <. map (intercalate " ")

strokeDashcorner :: String -> Styles
strokeDashcorner = declaration "stroke-dashcorner"

strokeDashoffset :: String -> Styles
strokeDashoffset = declaration "stroke-dashoffset"

strokeImage :: String -> Styles
strokeImage = declaration "stroke-image"

strokeImageJ :: Array String -> Styles
strokeImageJ = declaration "stroke-image" <. intercalate ", "

strokeLinecap :: String -> Styles
strokeLinecap = declaration "stroke-linecap"

strokeLinejoin :: String -> Styles
strokeLinejoin = declaration "stroke-linejoin"

strokeLinejoinJ :: Array String -> Styles
strokeLinejoinJ = declaration "stroke-linejoin" <. intercalate " "

strokeMiterlimit :: String -> Styles
strokeMiterlimit = declaration "stroke-miterlimit"

strokeOpacity :: String -> Styles
strokeOpacity = declaration "stroke-opacity"

strokeOrigin :: String -> Styles
strokeOrigin = declaration "stroke-origin"

strokePosition :: String -> Styles
strokePosition = declaration "stroke-position"

strokePositionJ :: Array String -> Styles
strokePositionJ = declaration "stroke-position" <. intercalate " "

strokePositionJJ :: Array (Array String) -> Styles
strokePositionJJ = declaration "stroke-position" <. intercalate ", " <. map (intercalate " ")

strokeRepeat :: String -> Styles
strokeRepeat = declaration "stroke-repeat"

strokeRepeatJ :: Array String -> Styles
strokeRepeatJ = declaration "stroke-repeat" <. intercalate " "

strokeRepeatJJ :: Array (Array String) -> Styles
strokeRepeatJJ = declaration "stroke-repeat" <. intercalate ", " <. map (intercalate " ")

strokeSize :: String -> Styles
strokeSize = declaration "stroke-size"

strokeSizeJ :: Array String -> Styles
strokeSizeJ = declaration "stroke-size" <. intercalate " "

strokeSizeJJ :: Array (Array String) -> Styles
strokeSizeJJ = declaration "stroke-size" <. intercalate ", " <. map (intercalate " ")

strokeWidth :: String -> Styles
strokeWidth = declaration "stroke-width"

strokeWidthJ :: Array String -> Styles
strokeWidthJ = declaration "stroke-width" <. intercalate ", "

tabSize :: String -> Styles
tabSize = declaration "tab-size"

tableLayout :: String -> Styles
tableLayout = declaration "table-layout"

textAlign :: String -> Styles
textAlign = declaration "text-align"

textAlignAll :: String -> Styles
textAlignAll = declaration "text-align-all"

textAlignLast :: String -> Styles
textAlignLast = declaration "text-align-last"

textCombineUpright :: String -> Styles
textCombineUpright = declaration "text-combine-upright"

textCombineUprightJ :: Array String -> Styles
textCombineUprightJ = declaration "text-combine-upright" <. intercalate " "

textDecoration :: String -> Styles
textDecoration = declaration "text-decoration"

textDecorationJ :: Array String -> Styles
textDecorationJ = declaration "text-decoration" <. intercalate " "

textDecorationJJ :: Array (Array String) -> Styles
textDecorationJJ = declaration "text-decoration" <. intercalate " " <. map (intercalate " ")

textDecorationColor :: String -> Styles
textDecorationColor = declaration "text-decoration-color"

textDecorationLine :: String -> Styles
textDecorationLine = declaration "text-decoration-line"

textDecorationLineJ :: Array String -> Styles
textDecorationLineJ = declaration "text-decoration-line" <. intercalate " "

textDecorationSkip :: String -> Styles
textDecorationSkip = declaration "text-decoration-skip"

textDecorationSkipJ :: Array String -> Styles
textDecorationSkipJ = declaration "text-decoration-skip" <. intercalate " "

textDecorationSkipInk :: String -> Styles
textDecorationSkipInk = declaration "text-decoration-skip-ink"

textDecorationStyle :: String -> Styles
textDecorationStyle = declaration "text-decoration-style"

textDecorationWidth :: String -> Styles
textDecorationWidth = declaration "text-decoration-width"

textEmphasis :: String -> Styles
textEmphasis = declaration "text-emphasis"

textEmphasisJ :: Array String -> Styles
textEmphasisJ = declaration "text-emphasis" <. intercalate " "

textEmphasisJJ :: Array (Array String) -> Styles
textEmphasisJJ = declaration "text-emphasis" <. intercalate " " <. map (intercalate " ")

textEmphasisColor :: String -> Styles
textEmphasisColor = declaration "text-emphasis-color"

textEmphasisPosition :: String -> Styles
textEmphasisPosition = declaration "text-emphasis-position"

textEmphasisPositionJ :: Array String -> Styles
textEmphasisPositionJ = declaration "text-emphasis-position" <. intercalate " "

textEmphasisSkip :: String -> Styles
textEmphasisSkip = declaration "text-emphasis-skip"

textEmphasisSkipJ :: Array String -> Styles
textEmphasisSkipJ = declaration "text-emphasis-skip" <. intercalate " "

textEmphasisStyle :: String -> Styles
textEmphasisStyle = declaration "text-emphasis-style"

textEmphasisStyleJ :: Array String -> Styles
textEmphasisStyleJ = declaration "text-emphasis-style" <. intercalate " "

textGroupAlign :: String -> Styles
textGroupAlign = declaration "text-group-align"

textIndent :: String -> Styles
textIndent = declaration "text-indent"

textIndentJ :: Array String -> Styles
textIndentJ = declaration "text-indent" <. intercalate " "

textJustify :: String -> Styles
textJustify = declaration "text-justify"

textOrientation :: String -> Styles
textOrientation = declaration "text-orientation"

textOverflow :: String -> Styles
textOverflow = declaration "text-overflow"

textShadow :: String -> Styles
textShadow = declaration "text-shadow"

textShadowJ :: Array String -> Styles
textShadowJ = declaration "text-shadow" <. intercalate " "

textShadowJJ :: Array (Array String) -> Styles
textShadowJJ = declaration "text-shadow" <. intercalate ", " <. map (intercalate " ")

textSpaceCollapse :: String -> Styles
textSpaceCollapse = declaration "text-space-collapse"

textSpaceTrim :: String -> Styles
textSpaceTrim = declaration "text-space-trim"

textSpaceTrimJ :: Array String -> Styles
textSpaceTrimJ = declaration "text-space-trim" <. intercalate " "

textSpacing :: String -> Styles
textSpacing = declaration "text-spacing"

textSpacingJ :: Array String -> Styles
textSpacingJ = declaration "text-spacing" <. intercalate " "

textTransform :: String -> Styles
textTransform = declaration "text-transform"

textTransformJ :: Array String -> Styles
textTransformJ = declaration "text-transform" <. intercalate " "

textUnderlineOffset :: String -> Styles
textUnderlineOffset = declaration "text-underline-offset"

textUnderlinePosition :: String -> Styles
textUnderlinePosition = declaration "text-underline-position"

textUnderlinePositionJ :: Array String -> Styles
textUnderlinePositionJ = declaration "text-underline-position" <. intercalate " "

textWrap :: String -> Styles
textWrap = declaration "text-wrap"

top :: String -> Styles
top = declaration "top"

transform :: String -> Styles
transform = declaration "transform"

transformJ :: Array String -> Styles
transformJ = declaration "transform" <. intercalate " "

transformBox :: String -> Styles
transformBox = declaration "transform-box"

transformOrigin :: String -> Styles
transformOrigin = declaration "transform-origin"

transformOriginJ :: Array String -> Styles
transformOriginJ = declaration "transform-origin" <. intercalate " "

transformStyle :: String -> Styles
transformStyle = declaration "transform-style"

transition :: String -> Styles
transition = declaration "transition"

transitionJ :: Array String -> Styles
transitionJ = declaration "transition" <. intercalate " "

transitionJJ :: Array (Array String) -> Styles
transitionJJ = declaration "transition" <. intercalate ", " <. map (intercalate " ")

transitionDelay :: String -> Styles
transitionDelay = declaration "transition-delay"

transitionDelayJ :: Array String -> Styles
transitionDelayJ = declaration "transition-delay" <. intercalate ", "

transitionDuration :: String -> Styles
transitionDuration = declaration "transition-duration"

transitionDurationJ :: Array String -> Styles
transitionDurationJ = declaration "transition-duration" <. intercalate ", "

transitionProperty :: String -> Styles
transitionProperty = declaration "transition-property"

transitionPropertyJ :: Array String -> Styles
transitionPropertyJ = declaration "transition-property" <. intercalate ", "

transitionTimingFunction :: String -> Styles
transitionTimingFunction = declaration "transition-timing-function"

transitionTimingFunctionJ :: Array String -> Styles
transitionTimingFunctionJ = declaration "transition-timing-function" <. intercalate ", "

translate :: String -> Styles
translate = declaration "translate"

translateJ :: Array String -> Styles
translateJ = declaration "translate" <. intercalate " "

unicodeBidi :: String -> Styles
unicodeBidi = declaration "unicode-bidi"

userSelect :: String -> Styles
userSelect = declaration "user-select"

verticalAlign :: String -> Styles
verticalAlign = declaration "vertical-align"

verticalAlignJ :: Array String -> Styles
verticalAlignJ = declaration "vertical-align" <. intercalate " "

visibility :: String -> Styles
visibility = declaration "visibility"

voiceBalance :: String -> Styles
voiceBalance = declaration "voice-balance"

voiceDuration :: String -> Styles
voiceDuration = declaration "voice-duration"

voiceFamily :: String -> Styles
voiceFamily = declaration "voice-family"

voiceFamilyJ :: Array String -> Styles
voiceFamilyJ = declaration "voice-family" <. intercalate " "

voiceFamilyJJ :: Array (Array String) -> Styles
voiceFamilyJJ = declaration "voice-family" <. intercalate ", " <. map (intercalate " ")

voicePitch :: String -> Styles
voicePitch = declaration "voice-pitch"

voicePitchJ :: Array String -> Styles
voicePitchJ = declaration "voice-pitch" <. intercalate " "

voiceRange :: String -> Styles
voiceRange = declaration "voice-range"

voiceRangeJ :: Array String -> Styles
voiceRangeJ = declaration "voice-range" <. intercalate " "

voiceRate :: String -> Styles
voiceRate = declaration "voice-rate"

voiceRateJ :: Array String -> Styles
voiceRateJ = declaration "voice-rate" <. intercalate " "

voiceStress :: String -> Styles
voiceStress = declaration "voice-stress"

voiceVolume :: String -> Styles
voiceVolume = declaration "voice-volume"

voiceVolumeJ :: Array String -> Styles
voiceVolumeJ = declaration "voice-volume" <. intercalate " "

volume :: String -> Styles
volume = declaration "volume"

whiteSpace :: String -> Styles
whiteSpace = declaration "white-space"

widows :: String -> Styles
widows = declaration "widows"

width :: String -> Styles
width = declaration "width"

willChange :: String -> Styles
willChange = declaration "will-change"

willChangeJ :: Array String -> Styles
willChangeJ = declaration "will-change" <. intercalate ", "

wordBoundaryDetection :: String -> Styles
wordBoundaryDetection = declaration "word-boundary-detection"

wordBoundaryExpansion :: String -> Styles
wordBoundaryExpansion = declaration "word-boundary-expansion"

wordBreak :: String -> Styles
wordBreak = declaration "word-break"

wordSpacing :: String -> Styles
wordSpacing = declaration "word-spacing"

wordWrap :: String -> Styles
wordWrap = declaration "word-wrap"

wrapAfter :: String -> Styles
wrapAfter = declaration "wrap-after"

wrapBefore :: String -> Styles
wrapBefore = declaration "wrap-before"

wrapFlow :: String -> Styles
wrapFlow = declaration "wrap-flow"

wrapInside :: String -> Styles
wrapInside = declaration "wrap-inside"

wrapThrough :: String -> Styles
wrapThrough = declaration "wrap-through"

writingMode :: String -> Styles
writingMode = declaration "writing-mode"

zIndex :: String -> Styles
zIndex = declaration "z-index"

-- Units
toUnit :: String -> Number -> String
toUnit suffix n = show n <> suffix

pct :: Number -> String
pct = toUnit "%"

em :: Number -> String
em = toUnit "em"

ex :: Number -> String
ex = toUnit "ex"

ch :: Number -> String
ch = toUnit "ch"

rem :: Number -> String
rem = toUnit "rem"

vw :: Number -> String
vw = toUnit "vw"

vh :: Number -> String
vh = toUnit "vh"

vmin :: Number -> String
vmin = toUnit "vmin"

vmax :: Number -> String
vmax = toUnit "vmax"

cm :: Number -> String
cm = toUnit "cm"

mm :: Number -> String
mm = toUnit "mm"

q :: Number -> String
q = toUnit "Q"

in_ :: Number -> String
in_ = toUnit "in"

pc :: Number -> String
pc = toUnit "pc"

pt :: Number -> String
pt = toUnit "pt"

px :: Number -> String
px = toUnit "px"

deg :: Number -> String
deg = toUnit "deg"

grad :: Number -> String
grad = toUnit "grad"

rad :: Number -> String
rad = toUnit "rad"

turn :: Number -> String
turn = toUnit "turn"

s :: Number -> String
s = toUnit "s"

ms :: Number -> String
ms = toUnit "ms"

hz :: Number -> String
hz = toUnit "Hz"

kHz :: Number -> String
kHz = toUnit "kHz"

dpi :: Number -> String
dpi = toUnit "dpi"

dpcm :: Number -> String
dpcm = toUnit "dpcm"

dppx :: Number -> String
dppx = toUnit "dppx"

fr :: Number -> String
fr = toUnit "fr"
