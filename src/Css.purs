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
alignContent = Single <. Declaration Id "align-content"

alignContentJ :: Array String -> Styles
alignContentJ = Single <. Declaration Id "align-content" <. intercalate " "

alignItems :: String -> Styles
alignItems = Single <. Declaration Id "align-items"

alignItemsJ :: Array String -> Styles
alignItemsJ = Single <. Declaration Id "align-items" <. intercalate " "

alignSelf :: String -> Styles
alignSelf = Single <. Declaration Id "align-self"

alignSelfJ :: Array String -> Styles
alignSelfJ = Single <. Declaration Id "align-self" <. intercalate " "

alignmentBaseline :: String -> Styles
alignmentBaseline = Single <. Declaration Id "alignment-baseline"

all :: String -> Styles
all = Single <. Declaration Id "all"

animation :: String -> Styles
animation = Single <. Declaration Id "animation"

animationJ :: Array String -> Styles
animationJ = Single <. Declaration Id "animation" <. intercalate " "

animationJJ :: Array (Array String) -> Styles
animationJJ = Single <. Declaration Id "animation" <. intercalate ", " <. map (intercalate " ")

animationDelay :: String -> Styles
animationDelay = Single <. Declaration Id "animation-delay"

animationDelayJ :: Array String -> Styles
animationDelayJ = Single <. Declaration Id "animation-delay" <. intercalate ", "

animationDirection :: String -> Styles
animationDirection = Single <. Declaration Id "animation-direction"

animationDirectionJ :: Array String -> Styles
animationDirectionJ = Single <. Declaration Id "animation-direction" <. intercalate ", "

animationDuration :: String -> Styles
animationDuration = Single <. Declaration Id "animation-duration"

animationDurationJ :: Array String -> Styles
animationDurationJ = Single <. Declaration Id "animation-duration" <. intercalate ", "

animationFillMode :: String -> Styles
animationFillMode = Single <. Declaration Id "animation-fill-mode"

animationFillModeJ :: Array String -> Styles
animationFillModeJ = Single <. Declaration Id "animation-fill-mode" <. intercalate ", "

animationIterationCount :: String -> Styles
animationIterationCount = Single <. Declaration Id "animation-iteration-count"

animationIterationCountJ :: Array String -> Styles
animationIterationCountJ = Single <. Declaration Id "animation-iteration-count" <. intercalate ", "

animationName :: String -> Styles
animationName = Single <. Declaration Id "animation-name"

animationNameJ :: Array String -> Styles
animationNameJ = Single <. Declaration Id "animation-name" <. intercalate ", "

animationPlayState :: String -> Styles
animationPlayState = Single <. Declaration Id "animation-play-state"

animationPlayStateJ :: Array String -> Styles
animationPlayStateJ = Single <. Declaration Id "animation-play-state" <. intercalate ", "

animationTimingFunction :: String -> Styles
animationTimingFunction = Single <. Declaration Id "animation-timing-function"

animationTimingFunctionJ :: Array String -> Styles
animationTimingFunctionJ = Single <. Declaration Id "animation-timing-function" <. intercalate ", "

appearance :: String -> Styles
appearance = Single <. Declaration Id "appearance"

azimuth :: String -> Styles
azimuth = Single <. Declaration Id "azimuth"

azimuthJ :: Array String -> Styles
azimuthJ = Single <. Declaration Id "azimuth" <. intercalate " "

backfaceVisibility :: String -> Styles
backfaceVisibility = Single <. Declaration Id "backface-visibility"

background :: String -> Styles
background = Single <. Declaration Id "background"

backgroundJ :: Array String -> Styles
backgroundJ = Single <. Declaration Id "background" <. intercalate " "

backgroundJJ :: Array (Array String) -> Styles
backgroundJJ = Single <. Declaration Id "background" <. intercalate ", " <. map (intercalate " ")

backgroundAttachment :: String -> Styles
backgroundAttachment = Single <. Declaration Id "background-attachment"

backgroundAttachmentJ :: Array String -> Styles
backgroundAttachmentJ = Single <. Declaration Id "background-attachment" <. intercalate ", "

backgroundBlendMode :: String -> Styles
backgroundBlendMode = Single <. Declaration Id "background-blend-mode"

backgroundClip :: String -> Styles
backgroundClip = Single <. Declaration Id "background-clip"

backgroundClipJ :: Array String -> Styles
backgroundClipJ = Single <. Declaration Id "background-clip" <. intercalate ", "

backgroundColor :: String -> Styles
backgroundColor = Single <. Declaration Id "background-color"

backgroundImage :: String -> Styles
backgroundImage = Single <. Declaration Id "background-image"

backgroundImageJ :: Array String -> Styles
backgroundImageJ = Single <. Declaration Id "background-image" <. intercalate ", "

backgroundOrigin :: String -> Styles
backgroundOrigin = Single <. Declaration Id "background-origin"

backgroundOriginJ :: Array String -> Styles
backgroundOriginJ = Single <. Declaration Id "background-origin" <. intercalate ", "

backgroundPosition :: String -> Styles
backgroundPosition = Single <. Declaration Id "background-position"

backgroundPositionJ :: Array String -> Styles
backgroundPositionJ = Single <. Declaration Id "background-position" <. intercalate " "

backgroundRepeat :: String -> Styles
backgroundRepeat = Single <. Declaration Id "background-repeat"

backgroundRepeatJ :: Array String -> Styles
backgroundRepeatJ = Single <. Declaration Id "background-repeat" <. intercalate " "

backgroundRepeatJJ :: Array (Array String) -> Styles
backgroundRepeatJJ = Single <. Declaration Id "background-repeat" <. intercalate ", " <. map (intercalate " ")

backgroundSize :: String -> Styles
backgroundSize = Single <. Declaration Id "background-size"

backgroundSizeJ :: Array String -> Styles
backgroundSizeJ = Single <. Declaration Id "background-size" <. intercalate " "

backgroundSizeJJ :: Array (Array String) -> Styles
backgroundSizeJJ = Single <. Declaration Id "background-size" <. intercalate ", " <. map (intercalate " ")

baselineShift :: String -> Styles
baselineShift = Single <. Declaration Id "baseline-shift"

blockOverflow :: String -> Styles
blockOverflow = Single <. Declaration Id "block-overflow"

blockSize :: String -> Styles
blockSize = Single <. Declaration Id "block-size"

blockStep :: String -> Styles
blockStep = Single <. Declaration Id "block-step"

blockStepJ :: Array String -> Styles
blockStepJ = Single <. Declaration Id "block-step" <. intercalate " "

blockStepAlign :: String -> Styles
blockStepAlign = Single <. Declaration Id "block-step-align"

blockStepInsert :: String -> Styles
blockStepInsert = Single <. Declaration Id "block-step-insert"

blockStepRound :: String -> Styles
blockStepRound = Single <. Declaration Id "block-step-round"

blockStepSize :: String -> Styles
blockStepSize = Single <. Declaration Id "block-step-size"

bookmarkLabel :: String -> Styles
bookmarkLabel = Single <. Declaration Id "bookmark-label"

bookmarkLabelJ :: Array String -> Styles
bookmarkLabelJ = Single <. Declaration Id "bookmark-label" <. intercalate " "

bookmarkLevel :: String -> Styles
bookmarkLevel = Single <. Declaration Id "bookmark-level"

bookmarkState :: String -> Styles
bookmarkState = Single <. Declaration Id "bookmark-state"

border :: String -> Styles
border = Single <. Declaration Id "border"

borderJ :: Array String -> Styles
borderJ = Single <. Declaration Id "border" <. intercalate " "

borderBlock :: String -> Styles
borderBlock = Single <. Declaration Id "border-block"

borderBlockJ :: Array String -> Styles
borderBlockJ = Single <. Declaration Id "border-block" <. intercalate " "

borderBlockColor :: String -> Styles
borderBlockColor = Single <. Declaration Id "border-block-color"

borderBlockColorJ :: Array String -> Styles
borderBlockColorJ = Single <. Declaration Id "border-block-color" <. intercalate " "

borderBlockEnd :: String -> Styles
borderBlockEnd = Single <. Declaration Id "border-block-end"

borderBlockEndJ :: Array String -> Styles
borderBlockEndJ = Single <. Declaration Id "border-block-end" <. intercalate " "

borderBlockEndColor :: String -> Styles
borderBlockEndColor = Single <. Declaration Id "border-block-end-color"

borderBlockEndStyle :: String -> Styles
borderBlockEndStyle = Single <. Declaration Id "border-block-end-style"

borderBlockEndWidth :: String -> Styles
borderBlockEndWidth = Single <. Declaration Id "border-block-end-width"

borderBlockStart :: String -> Styles
borderBlockStart = Single <. Declaration Id "border-block-start"

borderBlockStartJ :: Array String -> Styles
borderBlockStartJ = Single <. Declaration Id "border-block-start" <. intercalate " "

borderBlockStartColor :: String -> Styles
borderBlockStartColor = Single <. Declaration Id "border-block-start-color"

borderBlockStartStyle :: String -> Styles
borderBlockStartStyle = Single <. Declaration Id "border-block-start-style"

borderBlockStartWidth :: String -> Styles
borderBlockStartWidth = Single <. Declaration Id "border-block-start-width"

borderBlockStyle :: String -> Styles
borderBlockStyle = Single <. Declaration Id "border-block-style"

borderBlockStyleJ :: Array String -> Styles
borderBlockStyleJ = Single <. Declaration Id "border-block-style" <. intercalate " "

borderBlockWidth :: String -> Styles
borderBlockWidth = Single <. Declaration Id "border-block-width"

borderBlockWidthJ :: Array String -> Styles
borderBlockWidthJ = Single <. Declaration Id "border-block-width" <. intercalate " "

borderBottom :: String -> Styles
borderBottom = Single <. Declaration Id "border-bottom"

borderBottomJ :: Array String -> Styles
borderBottomJ = Single <. Declaration Id "border-bottom" <. intercalate " "

borderBottomColor :: String -> Styles
borderBottomColor = Single <. Declaration Id "border-bottom-color"

borderBottomLeftRadius :: String -> Styles
borderBottomLeftRadius = Single <. Declaration Id "border-bottom-left-radius"

borderBottomLeftRadiusJ :: Array String -> Styles
borderBottomLeftRadiusJ = Single <. Declaration Id "border-bottom-left-radius" <. intercalate " "

borderBottomRightRadius :: String -> Styles
borderBottomRightRadius = Single <. Declaration Id "border-bottom-right-radius"

borderBottomRightRadiusJ :: Array String -> Styles
borderBottomRightRadiusJ = Single <. Declaration Id "border-bottom-right-radius" <. intercalate " "

borderBottomStyle :: String -> Styles
borderBottomStyle = Single <. Declaration Id "border-bottom-style"

borderBottomWidth :: String -> Styles
borderBottomWidth = Single <. Declaration Id "border-bottom-width"

borderBoundary :: String -> Styles
borderBoundary = Single <. Declaration Id "border-boundary"

borderCollapse :: String -> Styles
borderCollapse = Single <. Declaration Id "border-collapse"

borderColor :: String -> Styles
borderColor = Single <. Declaration Id "border-color"

borderColorJ :: Array String -> Styles
borderColorJ = Single <. Declaration Id "border-color" <. intercalate " "

borderEndEndRadius :: String -> Styles
borderEndEndRadius = Single <. Declaration Id "border-end-end-radius"

borderEndEndRadiusJ :: Array String -> Styles
borderEndEndRadiusJ = Single <. Declaration Id "border-end-end-radius" <. intercalate " "

borderEndStartRadius :: String -> Styles
borderEndStartRadius = Single <. Declaration Id "border-end-start-radius"

borderEndStartRadiusJ :: Array String -> Styles
borderEndStartRadiusJ = Single <. Declaration Id "border-end-start-radius" <. intercalate " "

borderImage :: String -> Styles
borderImage = Single <. Declaration Id "border-image"

borderImageJ :: Array String -> Styles
borderImageJ = Single <. Declaration Id "border-image" <. intercalate " "

borderImageOutset :: String -> Styles
borderImageOutset = Single <. Declaration Id "border-image-outset"

borderImageOutsetJ :: Array String -> Styles
borderImageOutsetJ = Single <. Declaration Id "border-image-outset" <. intercalate " "

borderImageRepeat :: String -> Styles
borderImageRepeat = Single <. Declaration Id "border-image-repeat"

borderImageRepeatJ :: Array String -> Styles
borderImageRepeatJ = Single <. Declaration Id "border-image-repeat" <. intercalate " "

borderImageSlice :: String -> Styles
borderImageSlice = Single <. Declaration Id "border-image-slice"

borderImageSliceJ :: Array String -> Styles
borderImageSliceJ = Single <. Declaration Id "border-image-slice" <. intercalate " "

borderImageSource :: String -> Styles
borderImageSource = Single <. Declaration Id "border-image-source"

borderImageWidth :: String -> Styles
borderImageWidth = Single <. Declaration Id "border-image-width"

borderImageWidthJ :: Array String -> Styles
borderImageWidthJ = Single <. Declaration Id "border-image-width" <. intercalate " "

borderInline :: String -> Styles
borderInline = Single <. Declaration Id "border-inline"

borderInlineJ :: Array String -> Styles
borderInlineJ = Single <. Declaration Id "border-inline" <. intercalate " "

borderInlineColor :: String -> Styles
borderInlineColor = Single <. Declaration Id "border-inline-color"

borderInlineColorJ :: Array String -> Styles
borderInlineColorJ = Single <. Declaration Id "border-inline-color" <. intercalate " "

borderInlineEnd :: String -> Styles
borderInlineEnd = Single <. Declaration Id "border-inline-end"

borderInlineEndJ :: Array String -> Styles
borderInlineEndJ = Single <. Declaration Id "border-inline-end" <. intercalate " "

borderInlineEndColor :: String -> Styles
borderInlineEndColor = Single <. Declaration Id "border-inline-end-color"

borderInlineEndStyle :: String -> Styles
borderInlineEndStyle = Single <. Declaration Id "border-inline-end-style"

borderInlineEndWidth :: String -> Styles
borderInlineEndWidth = Single <. Declaration Id "border-inline-end-width"

borderInlineStart :: String -> Styles
borderInlineStart = Single <. Declaration Id "border-inline-start"

borderInlineStartJ :: Array String -> Styles
borderInlineStartJ = Single <. Declaration Id "border-inline-start" <. intercalate " "

borderInlineStartColor :: String -> Styles
borderInlineStartColor = Single <. Declaration Id "border-inline-start-color"

borderInlineStartStyle :: String -> Styles
borderInlineStartStyle = Single <. Declaration Id "border-inline-start-style"

borderInlineStartWidth :: String -> Styles
borderInlineStartWidth = Single <. Declaration Id "border-inline-start-width"

borderInlineStyle :: String -> Styles
borderInlineStyle = Single <. Declaration Id "border-inline-style"

borderInlineStyleJ :: Array String -> Styles
borderInlineStyleJ = Single <. Declaration Id "border-inline-style" <. intercalate " "

borderInlineWidth :: String -> Styles
borderInlineWidth = Single <. Declaration Id "border-inline-width"

borderInlineWidthJ :: Array String -> Styles
borderInlineWidthJ = Single <. Declaration Id "border-inline-width" <. intercalate " "

borderLeft :: String -> Styles
borderLeft = Single <. Declaration Id "border-left"

borderLeftJ :: Array String -> Styles
borderLeftJ = Single <. Declaration Id "border-left" <. intercalate " "

borderLeftColor :: String -> Styles
borderLeftColor = Single <. Declaration Id "border-left-color"

borderLeftStyle :: String -> Styles
borderLeftStyle = Single <. Declaration Id "border-left-style"

borderLeftWidth :: String -> Styles
borderLeftWidth = Single <. Declaration Id "border-left-width"

borderRadius :: String -> Styles
borderRadius = Single <. Declaration Id "border-radius"

borderRadiusJ :: Array String -> Styles
borderRadiusJ = Single <. Declaration Id "border-radius" <. intercalate " "

borderRight :: String -> Styles
borderRight = Single <. Declaration Id "border-right"

borderRightJ :: Array String -> Styles
borderRightJ = Single <. Declaration Id "border-right" <. intercalate " "

borderRightColor :: String -> Styles
borderRightColor = Single <. Declaration Id "border-right-color"

borderRightStyle :: String -> Styles
borderRightStyle = Single <. Declaration Id "border-right-style"

borderRightWidth :: String -> Styles
borderRightWidth = Single <. Declaration Id "border-right-width"

borderSpacing :: String -> Styles
borderSpacing = Single <. Declaration Id "border-spacing"

borderSpacingJ :: Array String -> Styles
borderSpacingJ = Single <. Declaration Id "border-spacing" <. intercalate " "

borderStartEndRadius :: String -> Styles
borderStartEndRadius = Single <. Declaration Id "border-start-end-radius"

borderStartEndRadiusJ :: Array String -> Styles
borderStartEndRadiusJ = Single <. Declaration Id "border-start-end-radius" <. intercalate " "

borderStartStartRadius :: String -> Styles
borderStartStartRadius = Single <. Declaration Id "border-start-start-radius"

borderStartStartRadiusJ :: Array String -> Styles
borderStartStartRadiusJ = Single <. Declaration Id "border-start-start-radius" <. intercalate " "

borderStyle :: String -> Styles
borderStyle = Single <. Declaration Id "border-style"

borderStyleJ :: Array String -> Styles
borderStyleJ = Single <. Declaration Id "border-style" <. intercalate " "

borderTop :: String -> Styles
borderTop = Single <. Declaration Id "border-top"

borderTopJ :: Array String -> Styles
borderTopJ = Single <. Declaration Id "border-top" <. intercalate " "

borderTopColor :: String -> Styles
borderTopColor = Single <. Declaration Id "border-top-color"

borderTopLeftRadius :: String -> Styles
borderTopLeftRadius = Single <. Declaration Id "border-top-left-radius"

borderTopLeftRadiusJ :: Array String -> Styles
borderTopLeftRadiusJ = Single <. Declaration Id "border-top-left-radius" <. intercalate " "

borderTopRightRadius :: String -> Styles
borderTopRightRadius = Single <. Declaration Id "border-top-right-radius"

borderTopRightRadiusJ :: Array String -> Styles
borderTopRightRadiusJ = Single <. Declaration Id "border-top-right-radius" <. intercalate " "

borderTopStyle :: String -> Styles
borderTopStyle = Single <. Declaration Id "border-top-style"

borderTopWidth :: String -> Styles
borderTopWidth = Single <. Declaration Id "border-top-width"

borderWidth :: String -> Styles
borderWidth = Single <. Declaration Id "border-width"

borderWidthJ :: Array String -> Styles
borderWidthJ = Single <. Declaration Id "border-width" <. intercalate " "

bottom :: String -> Styles
bottom = Single <. Declaration Id "bottom"

boxDecorationBreak :: String -> Styles
boxDecorationBreak = Single <. Declaration Id "box-decoration-break"

boxShadow :: String -> Styles
boxShadow = Single <. Declaration Id "box-shadow"

boxShadowJ :: Array String -> Styles
boxShadowJ = Single <. Declaration Id "box-shadow" <. intercalate " "

boxShadowJJ :: Array (Array String) -> Styles
boxShadowJJ = Single <. Declaration Id "box-shadow" <. intercalate ", " <. map (intercalate " ")

boxSizing :: String -> Styles
boxSizing = Single <. Declaration Id "box-sizing"

boxSnap :: String -> Styles
boxSnap = Single <. Declaration Id "box-snap"

breakAfter :: String -> Styles
breakAfter = Single <. Declaration Id "break-after"

breakBefore :: String -> Styles
breakBefore = Single <. Declaration Id "break-before"

breakInside :: String -> Styles
breakInside = Single <. Declaration Id "break-inside"

captionSide :: String -> Styles
captionSide = Single <. Declaration Id "caption-side"

caret :: String -> Styles
caret = Single <. Declaration Id "caret"

caretJ :: Array String -> Styles
caretJ = Single <. Declaration Id "caret" <. intercalate " "

caretColor :: String -> Styles
caretColor = Single <. Declaration Id "caret-color"

caretShape :: String -> Styles
caretShape = Single <. Declaration Id "caret-shape"

clear :: String -> Styles
clear = Single <. Declaration Id "clear"

clip :: String -> Styles
clip = Single <. Declaration Id "clip"

clipPath :: String -> Styles
clipPath = Single <. Declaration Id "clip-path"

clipPathJ :: Array String -> Styles
clipPathJ = Single <. Declaration Id "clip-path" <. intercalate " "

clipRule :: String -> Styles
clipRule = Single <. Declaration Id "clip-rule"

color :: String -> Styles
color = Single <. Declaration Id "color"

colorAdjust :: String -> Styles
colorAdjust = Single <. Declaration Id "color-adjust"

colorInterpolationFilters :: String -> Styles
colorInterpolationFilters = Single <. Declaration Id "color-interpolation-filters"

colorScheme :: String -> Styles
colorScheme = Single <. Declaration Id "color-scheme"

colorSchemeJ :: Array String -> Styles
colorSchemeJ = Single <. Declaration Id "color-scheme" <. intercalate " "

columnCount :: String -> Styles
columnCount = Single <. Declaration Id "column-count"

columnFill :: String -> Styles
columnFill = Single <. Declaration Id "column-fill"

columnGap :: String -> Styles
columnGap = Single <. Declaration Id "column-gap"

columnRule :: String -> Styles
columnRule = Single <. Declaration Id "column-rule"

columnRuleJ :: Array String -> Styles
columnRuleJ = Single <. Declaration Id "column-rule" <. intercalate " "

columnRuleColor :: String -> Styles
columnRuleColor = Single <. Declaration Id "column-rule-color"

columnRuleStyle :: String -> Styles
columnRuleStyle = Single <. Declaration Id "column-rule-style"

columnRuleWidth :: String -> Styles
columnRuleWidth = Single <. Declaration Id "column-rule-width"

columnSpan :: String -> Styles
columnSpan = Single <. Declaration Id "column-span"

columnWidth :: String -> Styles
columnWidth = Single <. Declaration Id "column-width"

columns :: String -> Styles
columns = Single <. Declaration Id "columns"

columnsJ :: Array String -> Styles
columnsJ = Single <. Declaration Id "columns" <. intercalate " "

contain :: String -> Styles
contain = Single <. Declaration Id "contain"

containJ :: Array String -> Styles
containJ = Single <. Declaration Id "contain" <. intercalate " "

content :: String -> Styles
content = Single <. Declaration Id "content"

contentJ :: Array String -> Styles
contentJ = Single <. Declaration Id "content" <. intercalate " "

continue :: String -> Styles
continue = Single <. Declaration Id "continue"

counterIncrement :: String -> Styles
counterIncrement = Single <. Declaration Id "counter-increment"

counterIncrementJ :: Array String -> Styles
counterIncrementJ = Single <. Declaration Id "counter-increment" <. intercalate " "

counterIncrementJJ :: Array (Array String) -> Styles
counterIncrementJJ = Single <. Declaration Id "counter-increment" <. intercalate " " <. map (intercalate " ")

counterReset :: String -> Styles
counterReset = Single <. Declaration Id "counter-reset"

counterResetJ :: Array String -> Styles
counterResetJ = Single <. Declaration Id "counter-reset" <. intercalate " "

counterResetJJ :: Array (Array String) -> Styles
counterResetJJ = Single <. Declaration Id "counter-reset" <. intercalate " " <. map (intercalate " ")

counterSet :: String -> Styles
counterSet = Single <. Declaration Id "counter-set"

counterSetJ :: Array String -> Styles
counterSetJ = Single <. Declaration Id "counter-set" <. intercalate " "

counterSetJJ :: Array (Array String) -> Styles
counterSetJJ = Single <. Declaration Id "counter-set" <. intercalate " " <. map (intercalate " ")

cue :: String -> Styles
cue = Single <. Declaration Id "cue"

cueJ :: Array String -> Styles
cueJ = Single <. Declaration Id "cue" <. intercalate " "

cueJJ :: Array (Array String) -> Styles
cueJJ = Single <. Declaration Id "cue" <. intercalate " " <. map (intercalate " ")

cueAfter :: String -> Styles
cueAfter = Single <. Declaration Id "cue-after"

cueAfterJ :: Array String -> Styles
cueAfterJ = Single <. Declaration Id "cue-after" <. intercalate " "

cueBefore :: String -> Styles
cueBefore = Single <. Declaration Id "cue-before"

cueBeforeJ :: Array String -> Styles
cueBeforeJ = Single <. Declaration Id "cue-before" <. intercalate " "

cursor :: String -> Styles
cursor = Single <. Declaration Id "cursor"

cursorJ :: Array String -> Styles
cursorJ = Single <. Declaration Id "cursor" <. intercalate " "

cursorJJ :: Array (Array String) -> Styles
cursorJJ = Single <. Declaration Id "cursor" <. intercalate " " <. map (intercalate " ")

direction :: String -> Styles
direction = Single <. Declaration Id "direction"

display :: String -> Styles
display = Single <. Declaration Id "display"

displayJ :: Array String -> Styles
displayJ = Single <. Declaration Id "display" <. intercalate " "

dominantBaseline :: String -> Styles
dominantBaseline = Single <. Declaration Id "dominant-baseline"

elevation :: String -> Styles
elevation = Single <. Declaration Id "elevation"

emptyCells :: String -> Styles
emptyCells = Single <. Declaration Id "empty-cells"

fill :: String -> Styles
fill = Single <. Declaration Id "fill"

fillJ :: Array String -> Styles
fillJ = Single <. Declaration Id "fill" <. intercalate " "

fillBreak :: String -> Styles
fillBreak = Single <. Declaration Id "fill-break"

fillColor :: String -> Styles
fillColor = Single <. Declaration Id "fill-color"

fillImage :: String -> Styles
fillImage = Single <. Declaration Id "fill-image"

fillImageJ :: Array String -> Styles
fillImageJ = Single <. Declaration Id "fill-image" <. intercalate ", "

fillOpacity :: String -> Styles
fillOpacity = Single <. Declaration Id "fill-opacity"

fillOrigin :: String -> Styles
fillOrigin = Single <. Declaration Id "fill-origin"

fillPosition :: String -> Styles
fillPosition = Single <. Declaration Id "fill-position"

fillPositionJ :: Array String -> Styles
fillPositionJ = Single <. Declaration Id "fill-position" <. intercalate " "

fillPositionJJ :: Array (Array String) -> Styles
fillPositionJJ = Single <. Declaration Id "fill-position" <. intercalate ", " <. map (intercalate " ")

fillRepeat :: String -> Styles
fillRepeat = Single <. Declaration Id "fill-repeat"

fillRepeatJ :: Array String -> Styles
fillRepeatJ = Single <. Declaration Id "fill-repeat" <. intercalate " "

fillRepeatJJ :: Array (Array String) -> Styles
fillRepeatJJ = Single <. Declaration Id "fill-repeat" <. intercalate ", " <. map (intercalate " ")

fillRule :: String -> Styles
fillRule = Single <. Declaration Id "fill-rule"

fillSize :: String -> Styles
fillSize = Single <. Declaration Id "fill-size"

fillSizeJ :: Array String -> Styles
fillSizeJ = Single <. Declaration Id "fill-size" <. intercalate " "

fillSizeJJ :: Array (Array String) -> Styles
fillSizeJJ = Single <. Declaration Id "fill-size" <. intercalate ", " <. map (intercalate " ")

filter :: String -> Styles
filter = Single <. Declaration Id "filter"

filterJ :: Array String -> Styles
filterJ = Single <. Declaration Id "filter" <. intercalate " "

flex :: String -> Styles
flex = Single <. Declaration Id "flex"

flexJ :: Array String -> Styles
flexJ = Single <. Declaration Id "flex" <. intercalate " "

flexBasis :: String -> Styles
flexBasis = Single <. Declaration Id "flex-basis"

flexDirection :: String -> Styles
flexDirection = Single <. Declaration Id "flex-direction"

flexFlow :: String -> Styles
flexFlow = Single <. Declaration Id "flex-flow"

flexFlowJ :: Array String -> Styles
flexFlowJ = Single <. Declaration Id "flex-flow" <. intercalate " "

flexGrow :: String -> Styles
flexGrow = Single <. Declaration Id "flex-grow"

flexShrink :: String -> Styles
flexShrink = Single <. Declaration Id "flex-shrink"

flexWrap :: String -> Styles
flexWrap = Single <. Declaration Id "flex-wrap"

float :: String -> Styles
float = Single <. Declaration Id "float"

floatDefer :: String -> Styles
floatDefer = Single <. Declaration Id "float-defer"

floatOffset :: String -> Styles
floatOffset = Single <. Declaration Id "float-offset"

floatReference :: String -> Styles
floatReference = Single <. Declaration Id "float-reference"

floodColor :: String -> Styles
floodColor = Single <. Declaration Id "flood-color"

floodOpacity :: String -> Styles
floodOpacity = Single <. Declaration Id "flood-opacity"

flowFrom :: String -> Styles
flowFrom = Single <. Declaration Id "flow-from"

flowInto :: String -> Styles
flowInto = Single <. Declaration Id "flow-into"

flowIntoJ :: Array String -> Styles
flowIntoJ = Single <. Declaration Id "flow-into" <. intercalate " "

font :: String -> Styles
font = Single <. Declaration Id "font"

fontJ :: Array String -> Styles
fontJ = Single <. Declaration Id "font" <. intercalate " "

fontFamily :: String -> Styles
fontFamily = Single <. Declaration Id "font-family"

fontFamilyJ :: Array String -> Styles
fontFamilyJ = Single <. Declaration Id "font-family" <. intercalate ", "

fontFeatureSettings :: String -> Styles
fontFeatureSettings = Single <. Declaration Id "font-feature-settings"

fontFeatureSettingsJ :: Array String -> Styles
fontFeatureSettingsJ = Single <. Declaration Id "font-feature-settings" <. intercalate " "

fontFeatureSettingsJJ :: Array (Array String) -> Styles
fontFeatureSettingsJJ = Single <. Declaration Id "font-feature-settings" <. intercalate ", " <. map (intercalate " ")

fontKerning :: String -> Styles
fontKerning = Single <. Declaration Id "font-kerning"

fontLanguageOverride :: String -> Styles
fontLanguageOverride = Single <. Declaration Id "font-language-override"

fontOpticalSizing :: String -> Styles
fontOpticalSizing = Single <. Declaration Id "font-optical-sizing"

fontPalette :: String -> Styles
fontPalette = Single <. Declaration Id "font-palette"

fontSize :: String -> Styles
fontSize = Single <. Declaration Id "font-size"

fontSizeAdjust :: String -> Styles
fontSizeAdjust = Single <. Declaration Id "font-size-adjust"

fontStretch :: String -> Styles
fontStretch = Single <. Declaration Id "font-stretch"

fontStyle :: String -> Styles
fontStyle = Single <. Declaration Id "font-style"

fontStyleJ :: Array String -> Styles
fontStyleJ = Single <. Declaration Id "font-style" <. intercalate " "

fontSynthesis :: String -> Styles
fontSynthesis = Single <. Declaration Id "font-synthesis"

fontSynthesisJ :: Array String -> Styles
fontSynthesisJ = Single <. Declaration Id "font-synthesis" <. intercalate " "

fontSynthesisSmallCaps :: String -> Styles
fontSynthesisSmallCaps = Single <. Declaration Id "font-synthesis-small-caps"

fontSynthesisStyle :: String -> Styles
fontSynthesisStyle = Single <. Declaration Id "font-synthesis-style"

fontSynthesisWeight :: String -> Styles
fontSynthesisWeight = Single <. Declaration Id "font-synthesis-weight"

fontVariant :: String -> Styles
fontVariant = Single <. Declaration Id "font-variant"

fontVariantJ :: Array String -> Styles
fontVariantJ = Single <. Declaration Id "font-variant" <. intercalate " "

fontVariantAlternates :: String -> Styles
fontVariantAlternates = Single <. Declaration Id "font-variant-alternates"

fontVariantAlternatesJ :: Array String -> Styles
fontVariantAlternatesJ = Single <. Declaration Id "font-variant-alternates" <. intercalate " "

fontVariantCaps :: String -> Styles
fontVariantCaps = Single <. Declaration Id "font-variant-caps"

fontVariantEastAsian :: String -> Styles
fontVariantEastAsian = Single <. Declaration Id "font-variant-east-asian"

fontVariantEastAsianJ :: Array String -> Styles
fontVariantEastAsianJ = Single <. Declaration Id "font-variant-east-asian" <. intercalate " "

fontVariantEmoji :: String -> Styles
fontVariantEmoji = Single <. Declaration Id "font-variant-emoji"

fontVariantLigatures :: String -> Styles
fontVariantLigatures = Single <. Declaration Id "font-variant-ligatures"

fontVariantLigaturesJ :: Array String -> Styles
fontVariantLigaturesJ = Single <. Declaration Id "font-variant-ligatures" <. intercalate " "

fontVariantNumeric :: String -> Styles
fontVariantNumeric = Single <. Declaration Id "font-variant-numeric"

fontVariantNumericJ :: Array String -> Styles
fontVariantNumericJ = Single <. Declaration Id "font-variant-numeric" <. intercalate " "

fontVariantPosition :: String -> Styles
fontVariantPosition = Single <. Declaration Id "font-variant-position"

fontVariationSettings :: String -> Styles
fontVariationSettings = Single <. Declaration Id "font-variation-settings"

fontVariationSettingsJ :: Array String -> Styles
fontVariationSettingsJ = Single <. Declaration Id "font-variation-settings" <. intercalate " "

fontVariationSettingsJJ :: Array (Array String) -> Styles
fontVariationSettingsJJ = Single <. Declaration Id "font-variation-settings" <. intercalate ", " <. map (intercalate " ")

fontWeight :: String -> Styles
fontWeight = Single <. Declaration Id "font-weight"

footnoteDisplay :: String -> Styles
footnoteDisplay = Single <. Declaration Id "footnote-display"

footnotePolicy :: String -> Styles
footnotePolicy = Single <. Declaration Id "footnote-policy"

forcedColorAdjust :: String -> Styles
forcedColorAdjust = Single <. Declaration Id "forced-color-adjust"

gap :: String -> Styles
gap = Single <. Declaration Id "gap"

gapJ :: Array String -> Styles
gapJ = Single <. Declaration Id "gap" <. intercalate " "

glyphOrientationVertical :: String -> Styles
glyphOrientationVertical = Single <. Declaration Id "glyph-orientation-vertical"

grid :: String -> Styles
grid = Single <. Declaration Id "grid"

gridJ :: Array String -> Styles
gridJ = Single <. Declaration Id "grid" <. intercalate " "

gridJJ :: Array (Array String) -> Styles
gridJJ = Single <. Declaration Id "grid" <. intercalate " " <. map (intercalate " ")

gridArea :: String -> Styles
gridArea = Single <. Declaration Id "grid-area"

gridAreaJ :: Array String -> Styles
gridAreaJ = Single <. Declaration Id "grid-area" <. intercalate " "

gridAreaJJ :: Array (Array String) -> Styles
gridAreaJJ = Single <. Declaration Id "grid-area" <. intercalate " " <. map (intercalate " ")

gridAutoColumns :: String -> Styles
gridAutoColumns = Single <. Declaration Id "grid-auto-columns"

gridAutoColumnsJ :: Array String -> Styles
gridAutoColumnsJ = Single <. Declaration Id "grid-auto-columns" <. intercalate " "

gridAutoFlow :: String -> Styles
gridAutoFlow = Single <. Declaration Id "grid-auto-flow"

gridAutoFlowJ :: Array String -> Styles
gridAutoFlowJ = Single <. Declaration Id "grid-auto-flow" <. intercalate " "

gridAutoRows :: String -> Styles
gridAutoRows = Single <. Declaration Id "grid-auto-rows"

gridAutoRowsJ :: Array String -> Styles
gridAutoRowsJ = Single <. Declaration Id "grid-auto-rows" <. intercalate " "

gridColumn :: String -> Styles
gridColumn = Single <. Declaration Id "grid-column"

gridColumnJ :: Array String -> Styles
gridColumnJ = Single <. Declaration Id "grid-column" <. intercalate " "

gridColumnJJ :: Array (Array String) -> Styles
gridColumnJJ = Single <. Declaration Id "grid-column" <. intercalate " " <. map (intercalate " ")

gridColumnEnd :: String -> Styles
gridColumnEnd = Single <. Declaration Id "grid-column-end"

gridColumnEndJ :: Array String -> Styles
gridColumnEndJ = Single <. Declaration Id "grid-column-end" <. intercalate " "

gridColumnStart :: String -> Styles
gridColumnStart = Single <. Declaration Id "grid-column-start"

gridColumnStartJ :: Array String -> Styles
gridColumnStartJ = Single <. Declaration Id "grid-column-start" <. intercalate " "

gridRow :: String -> Styles
gridRow = Single <. Declaration Id "grid-row"

gridRowJ :: Array String -> Styles
gridRowJ = Single <. Declaration Id "grid-row" <. intercalate " "

gridRowJJ :: Array (Array String) -> Styles
gridRowJJ = Single <. Declaration Id "grid-row" <. intercalate " " <. map (intercalate " ")

gridRowEnd :: String -> Styles
gridRowEnd = Single <. Declaration Id "grid-row-end"

gridRowEndJ :: Array String -> Styles
gridRowEndJ = Single <. Declaration Id "grid-row-end" <. intercalate " "

gridRowStart :: String -> Styles
gridRowStart = Single <. Declaration Id "grid-row-start"

gridRowStartJ :: Array String -> Styles
gridRowStartJ = Single <. Declaration Id "grid-row-start" <. intercalate " "

gridTemplate :: String -> Styles
gridTemplate = Single <. Declaration Id "grid-template"

gridTemplateJ :: Array String -> Styles
gridTemplateJ = Single <. Declaration Id "grid-template" <. intercalate " "

gridTemplateJJ :: Array (Array String) -> Styles
gridTemplateJJ = Single <. Declaration Id "grid-template" <. intercalate " " <. map (intercalate " ")

gridTemplateAreas :: String -> Styles
gridTemplateAreas = Single <. Declaration Id "grid-template-areas"

gridTemplateAreasJ :: Array String -> Styles
gridTemplateAreasJ = Single <. Declaration Id "grid-template-areas" <. intercalate " "

gridTemplateColumns :: String -> Styles
gridTemplateColumns = Single <. Declaration Id "grid-template-columns"

gridTemplateColumnsJ :: Array String -> Styles
gridTemplateColumnsJ = Single <. Declaration Id "grid-template-columns" <. intercalate " "

gridTemplateColumnsJJ :: Array (Array String) -> Styles
gridTemplateColumnsJJ = Single <. Declaration Id "grid-template-columns" <. intercalate " " <. map (intercalate " ")

gridTemplateRows :: String -> Styles
gridTemplateRows = Single <. Declaration Id "grid-template-rows"

gridTemplateRowsJ :: Array String -> Styles
gridTemplateRowsJ = Single <. Declaration Id "grid-template-rows" <. intercalate " "

gridTemplateRowsJJ :: Array (Array String) -> Styles
gridTemplateRowsJJ = Single <. Declaration Id "grid-template-rows" <. intercalate " " <. map (intercalate " ")

hangingPunctuation :: String -> Styles
hangingPunctuation = Single <. Declaration Id "hanging-punctuation"

hangingPunctuationJ :: Array String -> Styles
hangingPunctuationJ = Single <. Declaration Id "hanging-punctuation" <. intercalate " "

height :: String -> Styles
height = Single <. Declaration Id "height"

hyphenateCharacter :: String -> Styles
hyphenateCharacter = Single <. Declaration Id "hyphenate-character"

hyphenateLimitChars :: String -> Styles
hyphenateLimitChars = Single <. Declaration Id "hyphenate-limit-chars"

hyphenateLimitCharsJ :: Array String -> Styles
hyphenateLimitCharsJ = Single <. Declaration Id "hyphenate-limit-chars" <. intercalate " "

hyphenateLimitLast :: String -> Styles
hyphenateLimitLast = Single <. Declaration Id "hyphenate-limit-last"

hyphenateLimitLines :: String -> Styles
hyphenateLimitLines = Single <. Declaration Id "hyphenate-limit-lines"

hyphenateLimitZone :: String -> Styles
hyphenateLimitZone = Single <. Declaration Id "hyphenate-limit-zone"

hyphens :: String -> Styles
hyphens = Single <. Declaration Id "hyphens"

imageOrientation :: String -> Styles
imageOrientation = Single <. Declaration Id "image-orientation"

imageOrientationJ :: Array String -> Styles
imageOrientationJ = Single <. Declaration Id "image-orientation" <. intercalate " "

imageRendering :: String -> Styles
imageRendering = Single <. Declaration Id "image-rendering"

imageResolution :: String -> Styles
imageResolution = Single <. Declaration Id "image-resolution"

imageResolutionJ :: Array String -> Styles
imageResolutionJ = Single <. Declaration Id "image-resolution" <. intercalate " "

initialLetters :: String -> Styles
initialLetters = Single <. Declaration Id "initial-letters"

initialLettersJ :: Array String -> Styles
initialLettersJ = Single <. Declaration Id "initial-letters" <. intercalate " "

initialLettersAlign :: String -> Styles
initialLettersAlign = Single <. Declaration Id "initial-letters-align"

initialLettersAlignJ :: Array String -> Styles
initialLettersAlignJ = Single <. Declaration Id "initial-letters-align" <. intercalate " "

initialLettersWrap :: String -> Styles
initialLettersWrap = Single <. Declaration Id "initial-letters-wrap"

inlineSize :: String -> Styles
inlineSize = Single <. Declaration Id "inline-size"

inlineSizing :: String -> Styles
inlineSizing = Single <. Declaration Id "inline-sizing"

inset :: String -> Styles
inset = Single <. Declaration Id "inset"

insetJ :: Array String -> Styles
insetJ = Single <. Declaration Id "inset" <. intercalate " "

insetBlock :: String -> Styles
insetBlock = Single <. Declaration Id "inset-block"

insetBlockJ :: Array String -> Styles
insetBlockJ = Single <. Declaration Id "inset-block" <. intercalate " "

insetBlockEnd :: String -> Styles
insetBlockEnd = Single <. Declaration Id "inset-block-end"

insetBlockStart :: String -> Styles
insetBlockStart = Single <. Declaration Id "inset-block-start"

insetInline :: String -> Styles
insetInline = Single <. Declaration Id "inset-inline"

insetInlineJ :: Array String -> Styles
insetInlineJ = Single <. Declaration Id "inset-inline" <. intercalate " "

insetInlineEnd :: String -> Styles
insetInlineEnd = Single <. Declaration Id "inset-inline-end"

insetInlineStart :: String -> Styles
insetInlineStart = Single <. Declaration Id "inset-inline-start"

isolation :: String -> Styles
isolation = Single <. Declaration Id "isolation"

justifyContent :: String -> Styles
justifyContent = Single <. Declaration Id "justify-content"

justifyContentJ :: Array String -> Styles
justifyContentJ = Single <. Declaration Id "justify-content" <. intercalate " "

justifyItems :: String -> Styles
justifyItems = Single <. Declaration Id "justify-items"

justifyItemsJ :: Array String -> Styles
justifyItemsJ = Single <. Declaration Id "justify-items" <. intercalate " "

justifySelf :: String -> Styles
justifySelf = Single <. Declaration Id "justify-self"

justifySelfJ :: Array String -> Styles
justifySelfJ = Single <. Declaration Id "justify-self" <. intercalate " "

left :: String -> Styles
left = Single <. Declaration Id "left"

letterSpacing :: String -> Styles
letterSpacing = Single <. Declaration Id "letter-spacing"

lightingColor :: String -> Styles
lightingColor = Single <. Declaration Id "lighting-color"

lineBreak :: String -> Styles
lineBreak = Single <. Declaration Id "line-break"

lineClamp :: String -> Styles
lineClamp = Single <. Declaration Id "line-clamp"

lineClampJ :: Array String -> Styles
lineClampJ = Single <. Declaration Id "line-clamp" <. intercalate " "

lineGrid :: String -> Styles
lineGrid = Single <. Declaration Id "line-grid"

lineHeight :: String -> Styles
lineHeight = Single <. Declaration Id "line-height"

lineHeightStep :: String -> Styles
lineHeightStep = Single <. Declaration Id "line-height-step"

linePadding :: String -> Styles
linePadding = Single <. Declaration Id "line-padding"

lineSnap :: String -> Styles
lineSnap = Single <. Declaration Id "line-snap"

listStyle :: String -> Styles
listStyle = Single <. Declaration Id "list-style"

listStyleJ :: Array String -> Styles
listStyleJ = Single <. Declaration Id "list-style" <. intercalate " "

listStyleImage :: String -> Styles
listStyleImage = Single <. Declaration Id "list-style-image"

listStylePosition :: String -> Styles
listStylePosition = Single <. Declaration Id "list-style-position"

listStyleType :: String -> Styles
listStyleType = Single <. Declaration Id "list-style-type"

margin :: String -> Styles
margin = Single <. Declaration Id "margin"

marginJ :: Array String -> Styles
marginJ = Single <. Declaration Id "margin" <. intercalate " "

marginBlock :: String -> Styles
marginBlock = Single <. Declaration Id "margin-block"

marginBlockJ :: Array String -> Styles
marginBlockJ = Single <. Declaration Id "margin-block" <. intercalate " "

marginBlockEnd :: String -> Styles
marginBlockEnd = Single <. Declaration Id "margin-block-end"

marginBlockStart :: String -> Styles
marginBlockStart = Single <. Declaration Id "margin-block-start"

marginBottom :: String -> Styles
marginBottom = Single <. Declaration Id "margin-bottom"

marginBreak :: String -> Styles
marginBreak = Single <. Declaration Id "margin-break"

marginInline :: String -> Styles
marginInline = Single <. Declaration Id "margin-inline"

marginInlineJ :: Array String -> Styles
marginInlineJ = Single <. Declaration Id "margin-inline" <. intercalate " "

marginInlineEnd :: String -> Styles
marginInlineEnd = Single <. Declaration Id "margin-inline-end"

marginInlineStart :: String -> Styles
marginInlineStart = Single <. Declaration Id "margin-inline-start"

marginLeft :: String -> Styles
marginLeft = Single <. Declaration Id "margin-left"

marginRight :: String -> Styles
marginRight = Single <. Declaration Id "margin-right"

marginTop :: String -> Styles
marginTop = Single <. Declaration Id "margin-top"

marginTrim :: String -> Styles
marginTrim = Single <. Declaration Id "margin-trim"

marker :: String -> Styles
marker = Single <. Declaration Id "marker"

markerJ :: Array String -> Styles
markerJ = Single <. Declaration Id "marker" <. intercalate " "

markerEnd :: String -> Styles
markerEnd = Single <. Declaration Id "marker-end"

markerKnockoutLeft :: String -> Styles
markerKnockoutLeft = Single <. Declaration Id "marker-knockout-left"

markerKnockoutLeftJ :: Array String -> Styles
markerKnockoutLeftJ = Single <. Declaration Id "marker-knockout-left" <. intercalate " "

markerKnockoutRight :: String -> Styles
markerKnockoutRight = Single <. Declaration Id "marker-knockout-right"

markerKnockoutRightJ :: Array String -> Styles
markerKnockoutRightJ = Single <. Declaration Id "marker-knockout-right" <. intercalate " "

markerMid :: String -> Styles
markerMid = Single <. Declaration Id "marker-mid"

markerPattern :: String -> Styles
markerPattern = Single <. Declaration Id "marker-pattern"

markerPatternJ :: Array String -> Styles
markerPatternJ = Single <. Declaration Id "marker-pattern" <. intercalate " "

markerPatternJJ :: Array (Array String) -> Styles
markerPatternJJ = Single <. Declaration Id "marker-pattern" <. intercalate " " <. map (intercalate " ")

markerSegment :: String -> Styles
markerSegment = Single <. Declaration Id "marker-segment"

markerSide :: String -> Styles
markerSide = Single <. Declaration Id "marker-side"

markerStart :: String -> Styles
markerStart = Single <. Declaration Id "marker-start"

mask :: String -> Styles
mask = Single <. Declaration Id "mask"

maskJ :: Array String -> Styles
maskJ = Single <. Declaration Id "mask" <. intercalate " "

maskJJ :: Array (Array String) -> Styles
maskJJ = Single <. Declaration Id "mask" <. intercalate ", " <. map (intercalate " ")

maskBorder :: String -> Styles
maskBorder = Single <. Declaration Id "mask-border"

maskBorderJ :: Array String -> Styles
maskBorderJ = Single <. Declaration Id "mask-border" <. intercalate " "

maskBorderJJ :: Array (Array String) -> Styles
maskBorderJJ = Single <. Declaration Id "mask-border" <. intercalate " " <. map (intercalate " ")

maskBorderMode :: String -> Styles
maskBorderMode = Single <. Declaration Id "mask-border-mode"

maskBorderOutset :: String -> Styles
maskBorderOutset = Single <. Declaration Id "mask-border-outset"

maskBorderOutsetJ :: Array String -> Styles
maskBorderOutsetJ = Single <. Declaration Id "mask-border-outset" <. intercalate " "

maskBorderRepeat :: String -> Styles
maskBorderRepeat = Single <. Declaration Id "mask-border-repeat"

maskBorderRepeatJ :: Array String -> Styles
maskBorderRepeatJ = Single <. Declaration Id "mask-border-repeat" <. intercalate " "

maskBorderSlice :: String -> Styles
maskBorderSlice = Single <. Declaration Id "mask-border-slice"

maskBorderSliceJ :: Array String -> Styles
maskBorderSliceJ = Single <. Declaration Id "mask-border-slice" <. intercalate " "

maskBorderSliceJJ :: Array (Array String) -> Styles
maskBorderSliceJJ = Single <. Declaration Id "mask-border-slice" <. intercalate " " <. map (intercalate " ")

maskBorderSource :: String -> Styles
maskBorderSource = Single <. Declaration Id "mask-border-source"

maskBorderWidth :: String -> Styles
maskBorderWidth = Single <. Declaration Id "mask-border-width"

maskBorderWidthJ :: Array String -> Styles
maskBorderWidthJ = Single <. Declaration Id "mask-border-width" <. intercalate " "

maskClip :: String -> Styles
maskClip = Single <. Declaration Id "mask-clip"

maskClipJ :: Array String -> Styles
maskClipJ = Single <. Declaration Id "mask-clip" <. intercalate ", "

maskComposite :: String -> Styles
maskComposite = Single <. Declaration Id "mask-composite"

maskCompositeJ :: Array String -> Styles
maskCompositeJ = Single <. Declaration Id "mask-composite" <. intercalate ", "

maskImage :: String -> Styles
maskImage = Single <. Declaration Id "mask-image"

maskImageJ :: Array String -> Styles
maskImageJ = Single <. Declaration Id "mask-image" <. intercalate ", "

maskMode :: String -> Styles
maskMode = Single <. Declaration Id "mask-mode"

maskModeJ :: Array String -> Styles
maskModeJ = Single <. Declaration Id "mask-mode" <. intercalate ", "

maskOrigin :: String -> Styles
maskOrigin = Single <. Declaration Id "mask-origin"

maskOriginJ :: Array String -> Styles
maskOriginJ = Single <. Declaration Id "mask-origin" <. intercalate ", "

maskPosition :: String -> Styles
maskPosition = Single <. Declaration Id "mask-position"

maskPositionJ :: Array String -> Styles
maskPositionJ = Single <. Declaration Id "mask-position" <. intercalate ", "

maskRepeat :: String -> Styles
maskRepeat = Single <. Declaration Id "mask-repeat"

maskRepeatJ :: Array String -> Styles
maskRepeatJ = Single <. Declaration Id "mask-repeat" <. intercalate ", "

maskSize :: String -> Styles
maskSize = Single <. Declaration Id "mask-size"

maskSizeJ :: Array String -> Styles
maskSizeJ = Single <. Declaration Id "mask-size" <. intercalate ", "

maskType :: String -> Styles
maskType = Single <. Declaration Id "mask-type"

maxBlockSize :: String -> Styles
maxBlockSize = Single <. Declaration Id "max-block-size"

maxHeight :: String -> Styles
maxHeight = Single <. Declaration Id "max-height"

maxInlineSize :: String -> Styles
maxInlineSize = Single <. Declaration Id "max-inline-size"

maxLines :: String -> Styles
maxLines = Single <. Declaration Id "max-lines"

maxWidth :: String -> Styles
maxWidth = Single <. Declaration Id "max-width"

minBlockSize :: String -> Styles
minBlockSize = Single <. Declaration Id "min-block-size"

minHeight :: String -> Styles
minHeight = Single <. Declaration Id "min-height"

minInlineSize :: String -> Styles
minInlineSize = Single <. Declaration Id "min-inline-size"

minWidth :: String -> Styles
minWidth = Single <. Declaration Id "min-width"

mixBlendMode :: String -> Styles
mixBlendMode = Single <. Declaration Id "mix-blend-mode"

navDown :: String -> Styles
navDown = Single <. Declaration Id "nav-down"

navDownJ :: Array String -> Styles
navDownJ = Single <. Declaration Id "nav-down" <. intercalate " "

navLeft :: String -> Styles
navLeft = Single <. Declaration Id "nav-left"

navLeftJ :: Array String -> Styles
navLeftJ = Single <. Declaration Id "nav-left" <. intercalate " "

navRight :: String -> Styles
navRight = Single <. Declaration Id "nav-right"

navRightJ :: Array String -> Styles
navRightJ = Single <. Declaration Id "nav-right" <. intercalate " "

navUp :: String -> Styles
navUp = Single <. Declaration Id "nav-up"

navUpJ :: Array String -> Styles
navUpJ = Single <. Declaration Id "nav-up" <. intercalate " "

objectFit :: String -> Styles
objectFit = Single <. Declaration Id "object-fit"

objectPosition :: String -> Styles
objectPosition = Single <. Declaration Id "object-position"

objectPositionJ :: Array String -> Styles
objectPositionJ = Single <. Declaration Id "object-position" <. intercalate " "

offset :: String -> Styles
offset = Single <. Declaration Id "offset"

offsetJ :: Array String -> Styles
offsetJ = Single <. Declaration Id "offset" <. intercalate " "

offsetJJ :: Array (Array String) -> Styles
offsetJJ = Single <. Declaration Id "offset" <. intercalate " " <. map (intercalate " ")

offsetAfter :: String -> Styles
offsetAfter = Single <. Declaration Id "offset-after"

offsetAnchor :: String -> Styles
offsetAnchor = Single <. Declaration Id "offset-anchor"

offsetAnchorJ :: Array String -> Styles
offsetAnchorJ = Single <. Declaration Id "offset-anchor" <. intercalate " "

offsetBefore :: String -> Styles
offsetBefore = Single <. Declaration Id "offset-before"

offsetDistance :: String -> Styles
offsetDistance = Single <. Declaration Id "offset-distance"

offsetEnd :: String -> Styles
offsetEnd = Single <. Declaration Id "offset-end"

offsetPath :: String -> Styles
offsetPath = Single <. Declaration Id "offset-path"

offsetPathJ :: Array String -> Styles
offsetPathJ = Single <. Declaration Id "offset-path" <. intercalate " "

offsetPosition :: String -> Styles
offsetPosition = Single <. Declaration Id "offset-position"

offsetPositionJ :: Array String -> Styles
offsetPositionJ = Single <. Declaration Id "offset-position" <. intercalate " "

offsetRotate :: String -> Styles
offsetRotate = Single <. Declaration Id "offset-rotate"

offsetStart :: String -> Styles
offsetStart = Single <. Declaration Id "offset-start"

opacity :: String -> Styles
opacity = Single <. Declaration Id "opacity"

order :: String -> Styles
order = Single <. Declaration Id "order"

orphans :: String -> Styles
orphans = Single <. Declaration Id "orphans"

outline :: String -> Styles
outline = Single <. Declaration Id "outline"

outlineJ :: Array String -> Styles
outlineJ = Single <. Declaration Id "outline" <. intercalate " "

outlineColor :: String -> Styles
outlineColor = Single <. Declaration Id "outline-color"

outlineOffset :: String -> Styles
outlineOffset = Single <. Declaration Id "outline-offset"

outlineStyle :: String -> Styles
outlineStyle = Single <. Declaration Id "outline-style"

outlineWidth :: String -> Styles
outlineWidth = Single <. Declaration Id "outline-width"

overflow :: String -> Styles
overflow = Single <. Declaration Id "overflow"

overflowJ :: Array String -> Styles
overflowJ = Single <. Declaration Id "overflow" <. intercalate " "

overflowAnchor :: String -> Styles
overflowAnchor = Single <. Declaration Id "overflow-anchor"

overflowBlock :: String -> Styles
overflowBlock = Single <. Declaration Id "overflow-block"

overflowBlockJ :: Array String -> Styles
overflowBlockJ = Single <. Declaration Id "overflow-block" <. intercalate " "

overflowInline :: String -> Styles
overflowInline = Single <. Declaration Id "overflow-inline"

overflowInlineJ :: Array String -> Styles
overflowInlineJ = Single <. Declaration Id "overflow-inline" <. intercalate " "

overflowWrap :: String -> Styles
overflowWrap = Single <. Declaration Id "overflow-wrap"

overflowX :: String -> Styles
overflowX = Single <. Declaration Id "overflow-x"

overflowY :: String -> Styles
overflowY = Single <. Declaration Id "overflow-y"

overscrollBehavior :: String -> Styles
overscrollBehavior = Single <. Declaration Id "overscroll-behavior"

overscrollBehaviorJ :: Array String -> Styles
overscrollBehaviorJ = Single <. Declaration Id "overscroll-behavior" <. intercalate " "

overscrollBehaviorBlock :: String -> Styles
overscrollBehaviorBlock = Single <. Declaration Id "overscroll-behavior-block"

overscrollBehaviorInline :: String -> Styles
overscrollBehaviorInline = Single <. Declaration Id "overscroll-behavior-inline"

overscrollBehaviorX :: String -> Styles
overscrollBehaviorX = Single <. Declaration Id "overscroll-behavior-x"

overscrollBehaviorY :: String -> Styles
overscrollBehaviorY = Single <. Declaration Id "overscroll-behavior-y"

padding :: String -> Styles
padding = Single <. Declaration Id "padding"

paddingJ :: Array String -> Styles
paddingJ = Single <. Declaration Id "padding" <. intercalate " "

paddingBlock :: String -> Styles
paddingBlock = Single <. Declaration Id "padding-block"

paddingBlockJ :: Array String -> Styles
paddingBlockJ = Single <. Declaration Id "padding-block" <. intercalate " "

paddingBlockEnd :: String -> Styles
paddingBlockEnd = Single <. Declaration Id "padding-block-end"

paddingBlockStart :: String -> Styles
paddingBlockStart = Single <. Declaration Id "padding-block-start"

paddingBottom :: String -> Styles
paddingBottom = Single <. Declaration Id "padding-bottom"

paddingInline :: String -> Styles
paddingInline = Single <. Declaration Id "padding-inline"

paddingInlineJ :: Array String -> Styles
paddingInlineJ = Single <. Declaration Id "padding-inline" <. intercalate " "

paddingInlineEnd :: String -> Styles
paddingInlineEnd = Single <. Declaration Id "padding-inline-end"

paddingInlineStart :: String -> Styles
paddingInlineStart = Single <. Declaration Id "padding-inline-start"

paddingLeft :: String -> Styles
paddingLeft = Single <. Declaration Id "padding-left"

paddingRight :: String -> Styles
paddingRight = Single <. Declaration Id "padding-right"

paddingTop :: String -> Styles
paddingTop = Single <. Declaration Id "padding-top"

page :: String -> Styles
page = Single <. Declaration Id "page"

pageBreakAfter :: String -> Styles
pageBreakAfter = Single <. Declaration Id "page-break-after"

pageBreakBefore :: String -> Styles
pageBreakBefore = Single <. Declaration Id "page-break-before"

pageBreakInside :: String -> Styles
pageBreakInside = Single <. Declaration Id "page-break-inside"

pause :: String -> Styles
pause = Single <. Declaration Id "pause"

pauseJ :: Array String -> Styles
pauseJ = Single <. Declaration Id "pause" <. intercalate " "

pauseAfter :: String -> Styles
pauseAfter = Single <. Declaration Id "pause-after"

pauseBefore :: String -> Styles
pauseBefore = Single <. Declaration Id "pause-before"

perspective :: String -> Styles
perspective = Single <. Declaration Id "perspective"

perspectiveOrigin :: String -> Styles
perspectiveOrigin = Single <. Declaration Id "perspective-origin"

perspectiveOriginJ :: Array String -> Styles
perspectiveOriginJ = Single <. Declaration Id "perspective-origin" <. intercalate " "

pitch :: String -> Styles
pitch = Single <. Declaration Id "pitch"

pitchRange :: String -> Styles
pitchRange = Single <. Declaration Id "pitch-range"

placeContent :: String -> Styles
placeContent = Single <. Declaration Id "place-content"

placeContentJ :: Array String -> Styles
placeContentJ = Single <. Declaration Id "place-content" <. intercalate " "

placeItems :: String -> Styles
placeItems = Single <. Declaration Id "place-items"

placeItemsJ :: Array String -> Styles
placeItemsJ = Single <. Declaration Id "place-items" <. intercalate " "

placeSelf :: String -> Styles
placeSelf = Single <. Declaration Id "place-self"

placeSelfJ :: Array String -> Styles
placeSelfJ = Single <. Declaration Id "place-self" <. intercalate " "

playDuring :: String -> Styles
playDuring = Single <. Declaration Id "play-during"

playDuringJ :: Array String -> Styles
playDuringJ = Single <. Declaration Id "play-during" <. intercalate " "

pointerEvents :: String -> Styles
pointerEvents = Single <. Declaration Id "pointer-events"

position :: String -> Styles
position = Single <. Declaration Id "position"

quotes :: String -> Styles
quotes = Single <. Declaration Id "quotes"

quotesJ :: Array String -> Styles
quotesJ = Single <. Declaration Id "quotes" <. intercalate " "

quotesJJ :: Array (Array String) -> Styles
quotesJJ = Single <. Declaration Id "quotes" <. intercalate " " <. map (intercalate " ")

regionFragment :: String -> Styles
regionFragment = Single <. Declaration Id "region-fragment"

resize :: String -> Styles
resize = Single <. Declaration Id "resize"

rest :: String -> Styles
rest = Single <. Declaration Id "rest"

restJ :: Array String -> Styles
restJ = Single <. Declaration Id "rest" <. intercalate " "

restAfter :: String -> Styles
restAfter = Single <. Declaration Id "rest-after"

restBefore :: String -> Styles
restBefore = Single <. Declaration Id "rest-before"

richness :: String -> Styles
richness = Single <. Declaration Id "richness"

right :: String -> Styles
right = Single <. Declaration Id "right"

rotate :: String -> Styles
rotate = Single <. Declaration Id "rotate"

rotateJ :: Array String -> Styles
rotateJ = Single <. Declaration Id "rotate" <. intercalate " "

rowGap :: String -> Styles
rowGap = Single <. Declaration Id "row-gap"

rubyAlign :: String -> Styles
rubyAlign = Single <. Declaration Id "ruby-align"

rubyAlignJ :: Array String -> Styles
rubyAlignJ = Single <. Declaration Id "ruby-align" <. intercalate " "

rubyMerge :: String -> Styles
rubyMerge = Single <. Declaration Id "ruby-merge"

rubyPosition :: String -> Styles
rubyPosition = Single <. Declaration Id "ruby-position"

rubyPositionJ :: Array String -> Styles
rubyPositionJ = Single <. Declaration Id "ruby-position" <. intercalate " "

running :: String -> Styles
running = Single <. Declaration Id "running"

scale :: String -> Styles
scale = Single <. Declaration Id "scale"

scaleJ :: Array String -> Styles
scaleJ = Single <. Declaration Id "scale" <. intercalate " "

scrollBehavior :: String -> Styles
scrollBehavior = Single <. Declaration Id "scroll-behavior"

scrollMargin :: String -> Styles
scrollMargin = Single <. Declaration Id "scroll-margin"

scrollMarginJ :: Array String -> Styles
scrollMarginJ = Single <. Declaration Id "scroll-margin" <. intercalate " "

scrollMarginBlock :: String -> Styles
scrollMarginBlock = Single <. Declaration Id "scroll-margin-block"

scrollMarginBlockJ :: Array String -> Styles
scrollMarginBlockJ = Single <. Declaration Id "scroll-margin-block" <. intercalate " "

scrollMarginBlockEnd :: String -> Styles
scrollMarginBlockEnd = Single <. Declaration Id "scroll-margin-block-end"

scrollMarginBlockStart :: String -> Styles
scrollMarginBlockStart = Single <. Declaration Id "scroll-margin-block-start"

scrollMarginBottom :: String -> Styles
scrollMarginBottom = Single <. Declaration Id "scroll-margin-bottom"

scrollMarginInline :: String -> Styles
scrollMarginInline = Single <. Declaration Id "scroll-margin-inline"

scrollMarginInlineJ :: Array String -> Styles
scrollMarginInlineJ = Single <. Declaration Id "scroll-margin-inline" <. intercalate " "

scrollMarginInlineEnd :: String -> Styles
scrollMarginInlineEnd = Single <. Declaration Id "scroll-margin-inline-end"

scrollMarginInlineStart :: String -> Styles
scrollMarginInlineStart = Single <. Declaration Id "scroll-margin-inline-start"

scrollMarginLeft :: String -> Styles
scrollMarginLeft = Single <. Declaration Id "scroll-margin-left"

scrollMarginRight :: String -> Styles
scrollMarginRight = Single <. Declaration Id "scroll-margin-right"

scrollMarginTop :: String -> Styles
scrollMarginTop = Single <. Declaration Id "scroll-margin-top"

scrollPadding :: String -> Styles
scrollPadding = Single <. Declaration Id "scroll-padding"

scrollPaddingJ :: Array String -> Styles
scrollPaddingJ = Single <. Declaration Id "scroll-padding" <. intercalate " "

scrollPaddingBlock :: String -> Styles
scrollPaddingBlock = Single <. Declaration Id "scroll-padding-block"

scrollPaddingBlockJ :: Array String -> Styles
scrollPaddingBlockJ = Single <. Declaration Id "scroll-padding-block" <. intercalate " "

scrollPaddingBlockEnd :: String -> Styles
scrollPaddingBlockEnd = Single <. Declaration Id "scroll-padding-block-end"

scrollPaddingBlockStart :: String -> Styles
scrollPaddingBlockStart = Single <. Declaration Id "scroll-padding-block-start"

scrollPaddingBottom :: String -> Styles
scrollPaddingBottom = Single <. Declaration Id "scroll-padding-bottom"

scrollPaddingInline :: String -> Styles
scrollPaddingInline = Single <. Declaration Id "scroll-padding-inline"

scrollPaddingInlineJ :: Array String -> Styles
scrollPaddingInlineJ = Single <. Declaration Id "scroll-padding-inline" <. intercalate " "

scrollPaddingInlineEnd :: String -> Styles
scrollPaddingInlineEnd = Single <. Declaration Id "scroll-padding-inline-end"

scrollPaddingInlineStart :: String -> Styles
scrollPaddingInlineStart = Single <. Declaration Id "scroll-padding-inline-start"

scrollPaddingLeft :: String -> Styles
scrollPaddingLeft = Single <. Declaration Id "scroll-padding-left"

scrollPaddingRight :: String -> Styles
scrollPaddingRight = Single <. Declaration Id "scroll-padding-right"

scrollPaddingTop :: String -> Styles
scrollPaddingTop = Single <. Declaration Id "scroll-padding-top"

scrollSnapAlign :: String -> Styles
scrollSnapAlign = Single <. Declaration Id "scroll-snap-align"

scrollSnapAlignJ :: Array String -> Styles
scrollSnapAlignJ = Single <. Declaration Id "scroll-snap-align" <. intercalate " "

scrollSnapStop :: String -> Styles
scrollSnapStop = Single <. Declaration Id "scroll-snap-stop"

scrollSnapType :: String -> Styles
scrollSnapType = Single <. Declaration Id "scroll-snap-type"

scrollSnapTypeJ :: Array String -> Styles
scrollSnapTypeJ = Single <. Declaration Id "scroll-snap-type" <. intercalate " "

scrollbarColor :: String -> Styles
scrollbarColor = Single <. Declaration Id "scrollbar-color"

scrollbarColorJ :: Array String -> Styles
scrollbarColorJ = Single <. Declaration Id "scrollbar-color" <. intercalate " "

scrollbarGutter :: String -> Styles
scrollbarGutter = Single <. Declaration Id "scrollbar-gutter"

scrollbarGutterJ :: Array String -> Styles
scrollbarGutterJ = Single <. Declaration Id "scrollbar-gutter" <. intercalate " "

scrollbarWidth :: String -> Styles
scrollbarWidth = Single <. Declaration Id "scrollbar-width"

shapeImageThreshold :: String -> Styles
shapeImageThreshold = Single <. Declaration Id "shape-image-threshold"

shapeInside :: String -> Styles
shapeInside = Single <. Declaration Id "shape-inside"

shapeInsideJ :: Array String -> Styles
shapeInsideJ = Single <. Declaration Id "shape-inside" <. intercalate " "

shapeMargin :: String -> Styles
shapeMargin = Single <. Declaration Id "shape-margin"

shapeOutside :: String -> Styles
shapeOutside = Single <. Declaration Id "shape-outside"

shapeOutsideJ :: Array String -> Styles
shapeOutsideJ = Single <. Declaration Id "shape-outside" <. intercalate " "

spatialNavigationAction :: String -> Styles
spatialNavigationAction = Single <. Declaration Id "spatial-navigation-action"

spatialNavigationContain :: String -> Styles
spatialNavigationContain = Single <. Declaration Id "spatial-navigation-contain"

spatialNavigationFunction :: String -> Styles
spatialNavigationFunction = Single <. Declaration Id "spatial-navigation-function"

speak :: String -> Styles
speak = Single <. Declaration Id "speak"

speakAs :: String -> Styles
speakAs = Single <. Declaration Id "speak-as"

speakAsJ :: Array String -> Styles
speakAsJ = Single <. Declaration Id "speak-as" <. intercalate " "

speakHeader :: String -> Styles
speakHeader = Single <. Declaration Id "speak-header"

speakNumeral :: String -> Styles
speakNumeral = Single <. Declaration Id "speak-numeral"

speakPunctuation :: String -> Styles
speakPunctuation = Single <. Declaration Id "speak-punctuation"

speechRate :: String -> Styles
speechRate = Single <. Declaration Id "speech-rate"

stress :: String -> Styles
stress = Single <. Declaration Id "stress"

stringSet :: String -> Styles
stringSet = Single <. Declaration Id "string-set"

stringSetJ :: Array String -> Styles
stringSetJ = Single <. Declaration Id "string-set" <. intercalate " "

stringSetJJ :: Array (Array String) -> Styles
stringSetJJ = Single <. Declaration Id "string-set" <. intercalate ", " <. map (intercalate " ")

stroke :: String -> Styles
stroke = Single <. Declaration Id "stroke"

strokeJ :: Array String -> Styles
strokeJ = Single <. Declaration Id "stroke" <. intercalate " "

strokeJJ :: Array (Array String) -> Styles
strokeJJ = Single <. Declaration Id "stroke" <. intercalate ", " <. map (intercalate " ")

strokeAlign :: String -> Styles
strokeAlign = Single <. Declaration Id "stroke-align"

strokeAlignment :: String -> Styles
strokeAlignment = Single <. Declaration Id "stroke-alignment"

strokeBreak :: String -> Styles
strokeBreak = Single <. Declaration Id "stroke-break"

strokeColor :: String -> Styles
strokeColor = Single <. Declaration Id "stroke-color"

strokeColorJ :: Array String -> Styles
strokeColorJ = Single <. Declaration Id "stroke-color" <. intercalate ", "

strokeDashCorner :: String -> Styles
strokeDashCorner = Single <. Declaration Id "stroke-dash-corner"

strokeDashJustify :: String -> Styles
strokeDashJustify = Single <. Declaration Id "stroke-dash-justify"

strokeDashJustifyJ :: Array String -> Styles
strokeDashJustifyJ = Single <. Declaration Id "stroke-dash-justify" <. intercalate " "

strokeDashadjust :: String -> Styles
strokeDashadjust = Single <. Declaration Id "stroke-dashadjust"

strokeDashadjustJ :: Array String -> Styles
strokeDashadjustJ = Single <. Declaration Id "stroke-dashadjust" <. intercalate " "

strokeDasharray :: String -> Styles
strokeDasharray = Single <. Declaration Id "stroke-dasharray"

strokeDasharrayJ :: Array String -> Styles
strokeDasharrayJ = Single <. Declaration Id "stroke-dasharray" <. intercalate " "

strokeDasharrayJJ :: Array (Array String) -> Styles
strokeDasharrayJJ = Single <. Declaration Id "stroke-dasharray" <. intercalate ", " <. map (intercalate " ")

strokeDashcorner :: String -> Styles
strokeDashcorner = Single <. Declaration Id "stroke-dashcorner"

strokeDashoffset :: String -> Styles
strokeDashoffset = Single <. Declaration Id "stroke-dashoffset"

strokeImage :: String -> Styles
strokeImage = Single <. Declaration Id "stroke-image"

strokeImageJ :: Array String -> Styles
strokeImageJ = Single <. Declaration Id "stroke-image" <. intercalate ", "

strokeLinecap :: String -> Styles
strokeLinecap = Single <. Declaration Id "stroke-linecap"

strokeLinejoin :: String -> Styles
strokeLinejoin = Single <. Declaration Id "stroke-linejoin"

strokeLinejoinJ :: Array String -> Styles
strokeLinejoinJ = Single <. Declaration Id "stroke-linejoin" <. intercalate " "

strokeMiterlimit :: String -> Styles
strokeMiterlimit = Single <. Declaration Id "stroke-miterlimit"

strokeOpacity :: String -> Styles
strokeOpacity = Single <. Declaration Id "stroke-opacity"

strokeOrigin :: String -> Styles
strokeOrigin = Single <. Declaration Id "stroke-origin"

strokePosition :: String -> Styles
strokePosition = Single <. Declaration Id "stroke-position"

strokePositionJ :: Array String -> Styles
strokePositionJ = Single <. Declaration Id "stroke-position" <. intercalate " "

strokePositionJJ :: Array (Array String) -> Styles
strokePositionJJ = Single <. Declaration Id "stroke-position" <. intercalate ", " <. map (intercalate " ")

strokeRepeat :: String -> Styles
strokeRepeat = Single <. Declaration Id "stroke-repeat"

strokeRepeatJ :: Array String -> Styles
strokeRepeatJ = Single <. Declaration Id "stroke-repeat" <. intercalate " "

strokeRepeatJJ :: Array (Array String) -> Styles
strokeRepeatJJ = Single <. Declaration Id "stroke-repeat" <. intercalate ", " <. map (intercalate " ")

strokeSize :: String -> Styles
strokeSize = Single <. Declaration Id "stroke-size"

strokeSizeJ :: Array String -> Styles
strokeSizeJ = Single <. Declaration Id "stroke-size" <. intercalate " "

strokeSizeJJ :: Array (Array String) -> Styles
strokeSizeJJ = Single <. Declaration Id "stroke-size" <. intercalate ", " <. map (intercalate " ")

strokeWidth :: String -> Styles
strokeWidth = Single <. Declaration Id "stroke-width"

strokeWidthJ :: Array String -> Styles
strokeWidthJ = Single <. Declaration Id "stroke-width" <. intercalate ", "

tabSize :: String -> Styles
tabSize = Single <. Declaration Id "tab-size"

tableLayout :: String -> Styles
tableLayout = Single <. Declaration Id "table-layout"

textAlign :: String -> Styles
textAlign = Single <. Declaration Id "text-align"

textAlignAll :: String -> Styles
textAlignAll = Single <. Declaration Id "text-align-all"

textAlignLast :: String -> Styles
textAlignLast = Single <. Declaration Id "text-align-last"

textCombineUpright :: String -> Styles
textCombineUpright = Single <. Declaration Id "text-combine-upright"

textCombineUprightJ :: Array String -> Styles
textCombineUprightJ = Single <. Declaration Id "text-combine-upright" <. intercalate " "

textDecoration :: String -> Styles
textDecoration = Single <. Declaration Id "text-decoration"

textDecorationJ :: Array String -> Styles
textDecorationJ = Single <. Declaration Id "text-decoration" <. intercalate " "

textDecorationJJ :: Array (Array String) -> Styles
textDecorationJJ = Single <. Declaration Id "text-decoration" <. intercalate " " <. map (intercalate " ")

textDecorationColor :: String -> Styles
textDecorationColor = Single <. Declaration Id "text-decoration-color"

textDecorationLine :: String -> Styles
textDecorationLine = Single <. Declaration Id "text-decoration-line"

textDecorationLineJ :: Array String -> Styles
textDecorationLineJ = Single <. Declaration Id "text-decoration-line" <. intercalate " "

textDecorationSkip :: String -> Styles
textDecorationSkip = Single <. Declaration Id "text-decoration-skip"

textDecorationSkipJ :: Array String -> Styles
textDecorationSkipJ = Single <. Declaration Id "text-decoration-skip" <. intercalate " "

textDecorationSkipInk :: String -> Styles
textDecorationSkipInk = Single <. Declaration Id "text-decoration-skip-ink"

textDecorationStyle :: String -> Styles
textDecorationStyle = Single <. Declaration Id "text-decoration-style"

textDecorationWidth :: String -> Styles
textDecorationWidth = Single <. Declaration Id "text-decoration-width"

textEmphasis :: String -> Styles
textEmphasis = Single <. Declaration Id "text-emphasis"

textEmphasisJ :: Array String -> Styles
textEmphasisJ = Single <. Declaration Id "text-emphasis" <. intercalate " "

textEmphasisJJ :: Array (Array String) -> Styles
textEmphasisJJ = Single <. Declaration Id "text-emphasis" <. intercalate " " <. map (intercalate " ")

textEmphasisColor :: String -> Styles
textEmphasisColor = Single <. Declaration Id "text-emphasis-color"

textEmphasisPosition :: String -> Styles
textEmphasisPosition = Single <. Declaration Id "text-emphasis-position"

textEmphasisPositionJ :: Array String -> Styles
textEmphasisPositionJ = Single <. Declaration Id "text-emphasis-position" <. intercalate " "

textEmphasisSkip :: String -> Styles
textEmphasisSkip = Single <. Declaration Id "text-emphasis-skip"

textEmphasisSkipJ :: Array String -> Styles
textEmphasisSkipJ = Single <. Declaration Id "text-emphasis-skip" <. intercalate " "

textEmphasisStyle :: String -> Styles
textEmphasisStyle = Single <. Declaration Id "text-emphasis-style"

textEmphasisStyleJ :: Array String -> Styles
textEmphasisStyleJ = Single <. Declaration Id "text-emphasis-style" <. intercalate " "

textGroupAlign :: String -> Styles
textGroupAlign = Single <. Declaration Id "text-group-align"

textIndent :: String -> Styles
textIndent = Single <. Declaration Id "text-indent"

textIndentJ :: Array String -> Styles
textIndentJ = Single <. Declaration Id "text-indent" <. intercalate " "

textJustify :: String -> Styles
textJustify = Single <. Declaration Id "text-justify"

textOrientation :: String -> Styles
textOrientation = Single <. Declaration Id "text-orientation"

textOverflow :: String -> Styles
textOverflow = Single <. Declaration Id "text-overflow"

textShadow :: String -> Styles
textShadow = Single <. Declaration Id "text-shadow"

textShadowJ :: Array String -> Styles
textShadowJ = Single <. Declaration Id "text-shadow" <. intercalate " "

textShadowJJ :: Array (Array String) -> Styles
textShadowJJ = Single <. Declaration Id "text-shadow" <. intercalate ", " <. map (intercalate " ")

textSpaceCollapse :: String -> Styles
textSpaceCollapse = Single <. Declaration Id "text-space-collapse"

textSpaceTrim :: String -> Styles
textSpaceTrim = Single <. Declaration Id "text-space-trim"

textSpaceTrimJ :: Array String -> Styles
textSpaceTrimJ = Single <. Declaration Id "text-space-trim" <. intercalate " "

textSpacing :: String -> Styles
textSpacing = Single <. Declaration Id "text-spacing"

textSpacingJ :: Array String -> Styles
textSpacingJ = Single <. Declaration Id "text-spacing" <. intercalate " "

textTransform :: String -> Styles
textTransform = Single <. Declaration Id "text-transform"

textTransformJ :: Array String -> Styles
textTransformJ = Single <. Declaration Id "text-transform" <. intercalate " "

textUnderlineOffset :: String -> Styles
textUnderlineOffset = Single <. Declaration Id "text-underline-offset"

textUnderlinePosition :: String -> Styles
textUnderlinePosition = Single <. Declaration Id "text-underline-position"

textUnderlinePositionJ :: Array String -> Styles
textUnderlinePositionJ = Single <. Declaration Id "text-underline-position" <. intercalate " "

textWrap :: String -> Styles
textWrap = Single <. Declaration Id "text-wrap"

top :: String -> Styles
top = Single <. Declaration Id "top"

transform :: String -> Styles
transform = Single <. Declaration Id "transform"

transformJ :: Array String -> Styles
transformJ = Single <. Declaration Id "transform" <. intercalate " "

transformBox :: String -> Styles
transformBox = Single <. Declaration Id "transform-box"

transformOrigin :: String -> Styles
transformOrigin = Single <. Declaration Id "transform-origin"

transformOriginJ :: Array String -> Styles
transformOriginJ = Single <. Declaration Id "transform-origin" <. intercalate " "

transformStyle :: String -> Styles
transformStyle = Single <. Declaration Id "transform-style"

transition :: String -> Styles
transition = Single <. Declaration Id "transition"

transitionJ :: Array String -> Styles
transitionJ = Single <. Declaration Id "transition" <. intercalate " "

transitionJJ :: Array (Array String) -> Styles
transitionJJ = Single <. Declaration Id "transition" <. intercalate ", " <. map (intercalate " ")

transitionDelay :: String -> Styles
transitionDelay = Single <. Declaration Id "transition-delay"

transitionDelayJ :: Array String -> Styles
transitionDelayJ = Single <. Declaration Id "transition-delay" <. intercalate ", "

transitionDuration :: String -> Styles
transitionDuration = Single <. Declaration Id "transition-duration"

transitionDurationJ :: Array String -> Styles
transitionDurationJ = Single <. Declaration Id "transition-duration" <. intercalate ", "

transitionProperty :: String -> Styles
transitionProperty = Single <. Declaration Id "transition-property"

transitionPropertyJ :: Array String -> Styles
transitionPropertyJ = Single <. Declaration Id "transition-property" <. intercalate ", "

transitionTimingFunction :: String -> Styles
transitionTimingFunction = Single <. Declaration Id "transition-timing-function"

transitionTimingFunctionJ :: Array String -> Styles
transitionTimingFunctionJ = Single <. Declaration Id "transition-timing-function" <. intercalate ", "

translate :: String -> Styles
translate = Single <. Declaration Id "translate"

translateJ :: Array String -> Styles
translateJ = Single <. Declaration Id "translate" <. intercalate " "

unicodeBidi :: String -> Styles
unicodeBidi = Single <. Declaration Id "unicode-bidi"

userSelect :: String -> Styles
userSelect = Single <. Declaration Id "user-select"

verticalAlign :: String -> Styles
verticalAlign = Single <. Declaration Id "vertical-align"

verticalAlignJ :: Array String -> Styles
verticalAlignJ = Single <. Declaration Id "vertical-align" <. intercalate " "

visibility :: String -> Styles
visibility = Single <. Declaration Id "visibility"

voiceBalance :: String -> Styles
voiceBalance = Single <. Declaration Id "voice-balance"

voiceDuration :: String -> Styles
voiceDuration = Single <. Declaration Id "voice-duration"

voiceFamily :: String -> Styles
voiceFamily = Single <. Declaration Id "voice-family"

voiceFamilyJ :: Array String -> Styles
voiceFamilyJ = Single <. Declaration Id "voice-family" <. intercalate " "

voiceFamilyJJ :: Array (Array String) -> Styles
voiceFamilyJJ = Single <. Declaration Id "voice-family" <. intercalate ", " <. map (intercalate " ")

voicePitch :: String -> Styles
voicePitch = Single <. Declaration Id "voice-pitch"

voicePitchJ :: Array String -> Styles
voicePitchJ = Single <. Declaration Id "voice-pitch" <. intercalate " "

voiceRange :: String -> Styles
voiceRange = Single <. Declaration Id "voice-range"

voiceRangeJ :: Array String -> Styles
voiceRangeJ = Single <. Declaration Id "voice-range" <. intercalate " "

voiceRate :: String -> Styles
voiceRate = Single <. Declaration Id "voice-rate"

voiceRateJ :: Array String -> Styles
voiceRateJ = Single <. Declaration Id "voice-rate" <. intercalate " "

voiceStress :: String -> Styles
voiceStress = Single <. Declaration Id "voice-stress"

voiceVolume :: String -> Styles
voiceVolume = Single <. Declaration Id "voice-volume"

voiceVolumeJ :: Array String -> Styles
voiceVolumeJ = Single <. Declaration Id "voice-volume" <. intercalate " "

volume :: String -> Styles
volume = Single <. Declaration Id "volume"

whiteSpace :: String -> Styles
whiteSpace = Single <. Declaration Id "white-space"

widows :: String -> Styles
widows = Single <. Declaration Id "widows"

width :: String -> Styles
width = Single <. Declaration Id "width"

willChange :: String -> Styles
willChange = Single <. Declaration Id "will-change"

willChangeJ :: Array String -> Styles
willChangeJ = Single <. Declaration Id "will-change" <. intercalate ", "

wordBoundaryDetection :: String -> Styles
wordBoundaryDetection = Single <. Declaration Id "word-boundary-detection"

wordBoundaryExpansion :: String -> Styles
wordBoundaryExpansion = Single <. Declaration Id "word-boundary-expansion"

wordBreak :: String -> Styles
wordBreak = Single <. Declaration Id "word-break"

wordSpacing :: String -> Styles
wordSpacing = Single <. Declaration Id "word-spacing"

wordWrap :: String -> Styles
wordWrap = Single <. Declaration Id "word-wrap"

wrapAfter :: String -> Styles
wrapAfter = Single <. Declaration Id "wrap-after"

wrapBefore :: String -> Styles
wrapBefore = Single <. Declaration Id "wrap-before"

wrapFlow :: String -> Styles
wrapFlow = Single <. Declaration Id "wrap-flow"

wrapInside :: String -> Styles
wrapInside = Single <. Declaration Id "wrap-inside"

wrapThrough :: String -> Styles
wrapThrough = Single <. Declaration Id "wrap-through"

writingMode :: String -> Styles
writingMode = Single <. Declaration Id "writing-mode"

zIndex :: String -> Styles
zIndex = Single <. Declaration Id "z-index"

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
