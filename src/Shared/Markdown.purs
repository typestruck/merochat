module Shared.Markdown where

import Prelude

import Data.Array as DA
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Nullable (Nullable(..))
import Data.Nullable as DN
import Data.String as DS
import Data.String.CodeUnits as DSC
import Data.String.Pattern (Pattern(..), Replacement(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)

newtype Token = Token
      { "type" ∷ String
      , raw ∷ String
      , text ∷ String
      , tokens ∷ Nullable (Array Token)
      }

foreign import parseRestricted ∷ String → String

--second parameter is a wrapper function for event values
foreign import parse ∷ ∀ v w. String → (v → w) → String

foreign import lexer ∷ String → Array Token

markdown ∷ ∀ message. Array (NodeData message) → String → Html message
markdown attributes content = HE.createElement "markdown" (attributes <> [ HA.createAttribute "image-sync-load" "true", HA.createAttribute "content" $ imageContent content ]) []

newtype PreviewToken = PreviewToken
      { "type" ∷ String
      , raw ∷ String
      , text ∷ Nullable String
      , tokens ∷ Nullable (Array PreviewToken)
      , items ∷ Nullable (Array PreviewToken)
      , href ∷ Nullable String
      , title ∷ Nullable String
      }

foreign import previewLexer ∷ String → Array PreviewToken

restrictedMarkdown ∷ ∀ message. Array (NodeData message) → String → Html message
restrictedMarkdown attributes = previewLexer >>> previewTokens >>> HE.createElement "text" attributes

previewTokens ∷ ∀ message. Array PreviewToken → Array (Html message)
previewTokens tokens = DA.concatMap previewToken tokens

previewToken ∷ ∀ message. PreviewToken → Array (Html message)
previewToken (PreviewToken token) = case token."type" of
      "image" → [ styled "font-style" "italic" [ HE.text $ "Image file" <> if DS.null text then "" else ": " <> text ] ]
      "blockquote" → [ styled "font-style" "italic" [ HE.text "Quote:" ], HE.text " " ]
      "html" → if DS.contains (Pattern "<audio") $ DS.toLower token.raw then
            [ styled "font-style" "italic" [ HE.text "Audio" ], HE.text " " ]
            else []
      "strong" → [ styled "font-weight" "bold" children ]
      "em" → [ styled "font-style" "italic" children ]
      "del" → [ styled "text-decoration" "line-through" children ]
      "codespan" → [ styled "font-family" "monospace" [ HE.text text ] ]
      "code" → [ styled "font-family" "monospace" [ HE.text text ], HE.text " " ]
      "space" → [ HE.text " " ]
      "br" → [ HE.text " " ]
      "hr" → [ HE.text " " ]
      "list" → previewTokens <<< DM.fromMaybe [] $ DN.toMaybe token.items
      "list_item" → children <> [ HE.text " " ]
      _ → children
      where
      text = unescapePreview <<< DM.fromMaybe "" $ DN.toMaybe token.text
      children = case DN.toMaybe token.tokens of
            Just tokens → previewTokens tokens
            Nothing → [ HE.text text ]
      styled name value = HE.createElement "text" [ HA.style1 name value ]

unescapePreview ∷ String → String
unescapePreview = DS.replaceAll (Pattern "&lt;") (Replacement "<")
      >>> DS.replaceAll (Pattern "&gt;") (Replacement ">")
      >>> DS.replaceAll (Pattern "&quot;") (Replacement "\"")
      >>> DS.replaceAll (Pattern "&#39;") (Replacement "'")
      >>> DS.replaceAll (Pattern "&amp;") (Replacement "&")

imageContent ∷ String → String
imageContent content = replaceImageTokens (previewLexer content) content

replaceImageTokens ∷ Array PreviewToken → String → String
replaceImageTokens tokens content = case DA.uncons tokens of
      Nothing → content
      Just { head: token@(PreviewToken value), tail } → case DSC.indexOf (Pattern value.raw) content of
            Nothing → replaceImageTokens tail content
            Just index → DSC.take index content <> imageToken token <> replaceImageTokens tail (DSC.drop (index + DSC.length value.raw) content)

imageToken ∷ PreviewToken → String
imageToken (PreviewToken token) = case token."type" of
      "image" → case DN.toMaybe token.href >>= imageDimensions of
            Nothing → token.raw
            Just dimensions → "![" <> escapeImageText (DM.fromMaybe "" $ DN.toMaybe token.title) <> "](" <> dimensions.url <> " width=" <> show dimensions.width <> " height=" <> show dimensions.height <> caption <> ")"
      _ → replaceImageTokens children token.raw
      where
      children = DM.fromMaybe [] (DN.toMaybe token.tokens) <> DM.fromMaybe [] (DN.toMaybe token.items)
      caption = case DN.toMaybe token.text of
            Just text | not $ DS.null text → " \"" <> escapeImageText (unescapePreview text) <> "\""
            _ → ""

imageDimensions ∷ String → Maybe { width ∷ Int, height ∷ Int, url ∷ String }
imageDimensions href = do
      start ← DS.stripPrefix (Pattern "[") href
      end ← DSC.indexOf (Pattern "]") start
      dimensions ← case DS.split (Pattern ",") $ DSC.take end start of
            [ width, height ] → do
                  w ← DI.fromString width
                  h ← DI.fromString height
                  if w > 0 && h > 0 then Just { width: w, height: h } else Nothing
            _ → Nothing
      let url = DSC.drop (end + 1) start
      if DS.null url then Nothing else Just { width: dimensions.width, height: dimensions.height, url }

escapeImageText ∷ String → String
escapeImageText = DS.replaceAll (Pattern "\\") (Replacement "\\\\")
      >>> DS.replaceAll (Pattern "\"") (Replacement "\\\"")
      >>> DS.replaceAll (Pattern "[") (Replacement "\\[")
      >>> DS.replaceAll (Pattern "]") (Replacement "\\]")
