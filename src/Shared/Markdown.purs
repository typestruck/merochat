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
import Shared.Extra as SE

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
markdown attributes content = HE.div attributes [ profileMarkdown content ]

newtype PreviewToken = PreviewToken
      { "type" ∷ String
      , raw ∷ String
      , text ∷ Nullable String
      , tokens ∷ Nullable (Array PreviewToken)
      , items ∷ Nullable (Array PreviewToken)
      , href ∷ Nullable String
      , title ∷ Nullable String
      , ordered ∷ Nullable Boolean
      , start ∷ Nullable Int
      , header ∷ Nullable (Array PreviewToken)
      , rows ∷ Nullable (Array (Array PreviewToken))
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


profileMarkdown ∷ ∀ message. String → Html message
profileMarkdown = previewLexer >>> map themedBlock >>> HE.div [ HA.class' "profile-markdown" ]

themedBlock ∷ ∀ message. PreviewToken → Html message
themedBlock token@(PreviewToken value)
      | (value."type" == "paragraph" || value."type" == "html") && hasNativeMedia token =
              HE.createElement "markdown" [ HA.createAttribute "image-sync-load" "true", HA.createAttribute "content" $ imageContent value.raw ] []
      | otherwise = profileBlock token

hasNativeMedia ∷ PreviewToken → Boolean
hasNativeMedia (PreviewToken part) = part."type" == "image" || part."type" == "html" || DA.any hasNativeMedia (DM.fromMaybe [] $ DN.toMaybe part.tokens)

profileBlock ∷ ∀ message. PreviewToken → Html message
profileBlock token@(PreviewToken value) = case value."type" of
      "space" → HE.div_ []
      "list" → HE.div [ HA.class' "markdown-list" ] $ DA.mapWithIndex item (DM.fromMaybe [] $ DN.toMaybe value.items)
      "blockquote" → HE.div [ HA.class' "markdown-quote" ] $ map themedBlock tokens
      "table" → HE.div_ $
            [ tableRow true $ DM.fromMaybe [] (DN.toMaybe value.header) ] <> map (tableRow false) (DM.fromMaybe [] $ DN.toMaybe value.rows)
      "code" → HE.createElement "text" [ HA.class' "markdown-paragraph markdown-code" ] [ HE.text text ]
      "heading" → HE.createElement "text" [ HA.class' "markdown-heading" ] $ profileInline token
      "hr" → HE.div [ HA.class' "tag-ruler" ] []
      _ → HE.createElement "text" [ HA.class' "markdown-paragraph" ] $ profileInline token
      where
      tokens = DM.fromMaybe [] $ DN.toMaybe value.tokens
      text = unescapePreview <<< DM.fromMaybe "" $ DN.toMaybe value.text
      tableRow heading cells = HE.div [ HA.class' "markdown-table-row" ] $ map
            (profileInline >>> HE.createElement "text" [ HA.class' { "markdown-table-cell": true, "bold": heading } ]) cells
      item index (PreviewToken entry) = HE.div [ HA.class' "markdown-list-item" ]
            [ HE.createElement "text" [ HA.class' "markdown-list-marker" ]
                    [ HE.text $ if DM.fromMaybe false (DN.toMaybe value.ordered) then show (index + DM.fromMaybe 1 (DN.toMaybe value.start)) <> "." else "•" ]
            , HE.div [ HA.class' "markdown-list-content" ] $ map themedBlock (DM.fromMaybe [] $ DN.toMaybe entry.tokens)
            ]

profileInline ∷ ∀ message. PreviewToken → Array (Html message)
profileInline (PreviewToken value) = case value."type" of
      "strong" → [ styled "font-weight" "bold" children ]
      "em" → [ styled "font-style" "italic" children ]
      "del" → [ styled "text-decoration" "line-through" children ]
      "codespan" → [ styled "font-family" "monospace" [ HE.text text ] ]
      "br" → [ HE.text "\n" ]
      "link" → case DN.toMaybe value.href of
            Just href → [ HE.createElement "text" [ HA.class' "markdown-link", SE.externalLink href ] children ]
            Nothing → children
      "image" → case DN.toMaybe value.href of
            Just href → [ HE.createElement "inline-image"
                  [ HA.src $ DM.maybe href _.url (imageDimensions href)
                  , HA.class' "markdown-image"
                  , SE.externalLink $ DM.maybe href _.url (imageDimensions href)
                  ] [] ]
            Nothing → [ HE.text text ]
      "html" → []
      _ → children
      where
      text = unescapePreview <<< DM.fromMaybe "" $ DN.toMaybe value.text
      children = case DN.toMaybe value.tokens of
            Just tokens → DA.concatMap profileInline tokens
            Nothing → [ HE.text text ]
      styled name value = HE.createElement "text" [ HA.style1 name value ]
